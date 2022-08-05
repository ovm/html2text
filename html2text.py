# html2text - A simple module for converting HTML text content into
# plaintext and back to HTML.
#
# NOTE: We don't even attempt to follow any formatting in the document
# that doesn't directly translate to paragraphs and headings. You should
# only feed HTML text that is already in a line-by-line format to this
# module to get sane results. (it may work in other cases as well)

import re
import array

RE_COMMENT = re.compile("<!--.*?-->", re.S)
RE_XML_DCL = re.compile("<[!?][A-Z]+[^>]*>\s*", re.I)
RE_TAG_BEG = re.compile("<([A-Z0-9]+)[^>]*>\s*", re.I)
RE_TAG_END = re.compile("\s*<\/([A-Z0-9]+)>", re.I)
RE_AMP_ESC = re.compile("&([^;]+);")
RE_BKS_ESC = re.compile("\\\\([0-7]{3}|x[0-9A-F]{2}|u[0-9A-F]{4}|.)", re.I)
RE_SPACE   = re.compile("\s+")
RE_WORD    = re.compile("[^\s<>]+")

# ampersand escape sequences
AMP_SEQS = {
    "nbsp": " ", # sub with regular space
    "quot": "\"",
    "amp" : "&",
    "lt"  : "<",
    "gt"  : ">",
}

# backspace escape sequences
BKS_SEQS = {
    "a" : "\a",
    "b" : "\b",
    "f" : "\f",
    "n" : "\n",
    "r" : "\r",
    "t" : "\t",
    "v" : "\v"
}

# characters that are part of HTML (XML) syntax
HTML_SPECIAL_ENTITIES = {"<", ">", "&"}

# needed by t2h() for encoding
HTML_SPECIAL_ENTITY_SEQS = {}
for ent in HTML_SPECIAL_ENTITIES:
    HTML_SPECIAL_ENTITY_SEQS[ent] = next((seq \
        for seq, _ent in AMP_SEQS.items() if _ent == ent), None)

# tags that don't have a terminating tag
HTML_UNARY_TAGS = {"br", "hr", "img"}

# tags that are used for inline text formatting
# (only thing special about these is that we
#  consider the space around the tags)
# <span> is here only to help with EPUBs, as some
# of them use CSS rules instead of the matching
# HTML elements to format text.
HTML_FORMATTING_TAGS = {"b", "strong", "i", "em", "span"}

# tags whose contents aren't displayed normally
# (you can also add tags that should be completely
#  ignore while parsing here)
HTML_HIDDEN_TAGS = {"head", "script", "style"}

# tags that are parsed even if their parent
# tag is in HTML_HIDDEN_TAGS
# (currently we extract a title from <head> this way)
HTML_IMPORTANT_TAGS = {"title"}

# I would like to note here, that if python had goto
# the indentation here could be cut at least two levels.
# (without using niggerlicious OOP patterns)
def h2t(html, line_width=0):
    # count the line breaks at the end of text
    def cntlbs(text):
        i = len(text)
        j = 0
        while i > 0:
            i -= 1
            if text[i] != "\n":
                break
            j += 1
        return j

    # substitute character in place of ampersand escape sequence
    def amp_sub(m, decode_special_entities=False):
        # capturing group 1 contains the sequence
        seq = m.group(1)

        # known escape sequence?
        if seq in AMP_SEQS:
            sub = AMP_SEQS[seq]

        # numeric representation of a character?
        elif seq[0] == "#":
            sub = seq[1:]

            # conver to unicode char
            if sub.isdigit():
                sub = chr(int(sub, 10))
            # we strip invalid sequences
            else:
                sub = ""

        # not a sequence we care about
        # FIXME: this may remove something important!
        # --> implement more escape sequences
        else:
            sub = ""

        # are we ignoring special HTML entities
        if not decode_special_entities and \
            sub in HTML_SPECIAL_ENTITIES:
            return m.group(0)

        return sub

    # substitute character in place of backspace escape sequence
    def bks_sub(m, decode_special_entities=False):
        # capturing group 1 contains the sequence
        seq = m.group(1)

        # known escape sequence
        if seq in BKS_SEQS:
            sub = BKS_SEQS[seq]

        # numeric sequence
        elif seq.isdigit():
            # octal byte?
            if len(seq) == 3:
                sub = chr(int(seq, 8))

            # single number?
            elif len(seq) == 1:
                sub = chr(int(seq, 10))

        # hex byte?
        elif (seq[0] == "x") and (len(seq) == 3):
            sub = chr(int(seq[1:], 16))

        # unicode char?
        elif (seq[0] == "u") and (len(seq) == 5):
            sub = chr(int(seq[1:], 16))

        # just return the escaped character
        # this handles quotes and backspaces
        else:
            sub = seq

        # are we ignoring special HTML entities
        if not decode_special_entities and \
            sub in HTML_SPECIAL_ENTITIES:
            return m.group(0)

        return sub

    def extend_at(a, i, w):
        # use extend() when possible
        if i >= (len(a) - 1):
            a.extend(w)

        # otherwise insert character by character
        else:
            j = len(w)
            while j > 0:
                j -= 1
                a.insert(i, w[j])

    # output buffer
    text_buffer = array.array("u")

    # number of characters in the current line
    line_chars = 0

    # NOTE: these are currently unused
    #num_heads = 0
    #num_pgrps = 0

    # keeps track of tag nesting
    tag_stack = []

    # if we've hit whitespace between words
    # (this is hacked into the logic and
    # may not work consistently)
    had_space = False

    # the contents of a <title> element have been inserted
    # as the first paragraph. this makes sure that content
    # which didn't originate inside a <title> element isn't
    # overwritten. (NOTE: unimplemented at the moment)
    #has_title = False

    # if we hit a tag whose content's should be ignored
    # the index in tag_stack is stored here. we discard
    # all tags/words/spaces until a matching terminating
    # tag is encountered, or if tag_stack couldn't contain
    # a terminating tag
    hiding_pos = -1
    hiding_tag = None

    # pre-process to substitute escape sequences with the
    # characters they represent. we ignore special HTML
    # entities here as decoding them here effects parsing
    html = RE_AMP_ESC.sub(amp_sub, RE_BKS_ESC.sub(bks_sub, html))

    i = 0
    while i < len(html):
        # is this a comment?
        match = RE_COMMENT.match(html, i)
        if match:
            # strip it out
            i = match.end(0)
            continue

        # XML declaration (also handles DOCTYPE)
        match = RE_XML_DCL.match(html, i)
        if match:
            # strip it out
            i = match.end(0)
            continue

        # is this the start of a tag?
        match = RE_TAG_BEG.match(html, i)
        if match:
            # capturing group 1 contains the tag name
            tag = match.group(1).lower()

            # append the tag to the tag_stack to keep track
            # of when we should expect a terminating tag
            if not tag in HTML_UNARY_TAGS:
                # if we're already hiding due to a previously
                # encountered tag, we wait to encounter that
                # tag's terminating tag
                if hiding_pos == -1:
                    # is this tag hiding it's contents
                    if (tag in HTML_HIDDEN_TAGS):
                        hiding_pos = len(tag_stack)
                        hiding_tag = tag

                # this should only be reached if the current
                # hiding tag is missing a terminating tag
                # ie. the inpout HTML is malformed
                elif (hiding_pos >= len(tag_stack)) \
                    or (tag_stack[hiding_pos] != hiding_tag):
                    hiding_pos = -1
                    hiding_tag = None

                tag_stack.append(tag)

            if (hiding_pos == -1) or (tag in HTML_IMPORTANT_TAGS):
                # any heading tag
                if (len(tag) == 2) and (tag[0] == "h") \
                    and (tag[1].isdigit()):
                    # terminate possible line
                    if line_chars > 0:
                        text_buffer.append("\n")
                        line_chars = 0
                    if len(text_buffer) > 0:
                        # consistent separation
                        count = 3 - cntlbs(text_buffer)
                        if count > 0:
                            text_buffer.extend("\n"*count)

                # paragraph tag
                elif tag == "p":
                    # terminate possible line
                    if line_chars > 0:
                        text_buffer.append("\n")
                        line_chars = 0

                # line break
                elif tag == "br":
                    # terminate possible line
                    if line_chars > 0:
                        text_buffer.append("\n")
                        line_chars = 0

                    count = 2 - cntlbs(text_buffer)
                    if count > 0:
                        text_buffer.extend("\n"*count)

                # inline text formatting tag
                elif tag in HTML_FORMATTING_TAGS:
                    # we consider space around inline
                    # formatting tags (unlike others)
                    if match.group(0)[-1].isspace():
                        had_space = True

                # other tags don't effect text formatting
                else:
                    pass

            # skip over the tag
            i = match.end(0)
            continue

        # is this the end of a tag?
        match = RE_TAG_END.match(html, i)
        if match:
            # captuing group 1 contains the tag name
            tag = match.group(1).lower()

            while 1:
                # stray terminating tag, ignore
                if len(tag_stack) < 1:
                    break

                tmp = tag_stack.pop()

                # terminating tag for the hiding tag
                if (tmp == hiding_tag) \
                    and (hiding_pos == len(tag_stack)):
                    hiding_pos = -1

                ### FIXME:
                ### a validation check for the hiding tag here may
                ### result in better formatting of malformed HTML

                if (hiding_pos == -1) or (tag in HTML_IMPORTANT_TAGS):
                    # any heading tag
                    if (len(tmp) == 2) and (tmp[0] == "h") \
                        and (tmp[1].isdigit()):
                        # terminate possible line
                        if line_chars > 0:
                            text_buffer.append("\n")
                            line_chars = 0

                        if len(text_buffer) > 0:
                            # this is a fix for empty heading tags
                            # so that the next tag doesn't get
                            # interpreted as a heading if it isn't one
                            count = cntlbs(text_buffer)
                            if count > 2:
                                del text_buffer[-(count - 2):]
                            else:
                                # consistent separation
                                text_buffer.extend("\n"*(2 - count))

                    # paragraph tag
                    elif tmp == "p":
                        # terminate possible line
                        if line_chars > 0:
                            text_buffer.append("\n")
                            line_chars = 0

                        if len(text_buffer) > 0:
                            # consistent separation
                            count = 2 - cntlbs(text_buffer)
                            if count > 0:
                                text_buffer.extend("\n"*count)

                    # inline text formatting tag
                    elif tmp in HTML_FORMATTING_TAGS:
                        # we consider space around inline
                        # formatting tags (unlike others)
                        if match.group(0)[0].isspace():
                            had_space = True

                    # other tags don't effect text formatting
                    else:
                        pass

                # tag for this terminating tag:
                if tmp == tag:
                    break

            i = match.end(0)
            continue

        # don't even attempt to parse words/space
        # if the tag contents are hidden
        if hiding_pos != -1 and ((len(tag_stack) < 1) \
            or not tag_stack[-1] in HTML_IMPORTANT_TAGS):
            i += 1
            continue

        # strip whitespace
        match = RE_SPACE.match(html, i)
        if match:
            # indicate that we should separate
            # the next word with a space
            had_space = True

            i = match.end(0)
            continue

        # extract a single word
        match = RE_WORD.match(html, i)
        if match:
            word = match.group(0)

            # decode special HTML entities (if any exist)
            word = RE_AMP_ESC.sub(lambda x: amp_sub(x, True), \
                RE_BKS_ESC.sub(lambda x: bks_sub(x, True), word))

            # if the line size is limited, break the line as necessary
            # if line_width is less than 2, we get a divison by zero
            if line_width > 1:
                break_char = False
                break_word = False

                # if the word is longer than a single
                # line we'll always want to break by character
                if len(word) > line_width:
                    break_char = True

                # if the word will make the line too long AND
                # the line won't be left at most 50% empty
                # start a new line, otherwise split by character
                # to fill the empty space
                elif (len(word) + line_chars + 1) > line_width:
                    if (line_chars / line_width) < 0.5:
                        break_char = True
                    else:
                        break_word = True

                # break the word as many times as needed
                if break_char:
                    # if the line contains text
                    # we'll attempt to fit a part
                    # of this word onto the line
                    if line_chars > 0:
                        left = line_width - line_chars

                        # at which threshold should we not
                        # attempt this. I think 6 characters
                        # is fine (2 are reserved for the space
                        # and for the dash)
                        if left > 6:
                            break_point = left - (2 if had_space else 1)
                            piece = word[:break_point]

                            if had_space:
                                text_buffer.append(" ")
                                line_chars += 1

                            text_buffer.extend(piece)
                            text_buffer.append("-")
                            line_chars += len(piece) + 1

                        else:
                            break_point = 0

                        text_buffer.append("\n")
                        line_chars = 0

                    else:
                        break_point = 0

                    # how many breaks do we need
                    breaks = (len(word) - break_point) // (line_width - 1)
                    while 1:
                        piece = word[break_point:break_point + line_width - 1]

                        # append piece
                        text_buffer.extend(piece)

                        # are we done?
                        if breaks < 1:
                            # this is the final piece
                            line_chars += len(piece)
                            break

                        # there will be another break
                        text_buffer.extend("-\n")

                        break_point += line_width - 1
                        breaks -= 1

                # otherwise if the word can't fit on the current
                # line, start a new line with the word
                elif break_word:
                    # the line has an extra space at the end
                    # we don't want it in the output
                    text_buffer.append("\n")
                    text_buffer.extend(word)
                    line_chars = len(word)

                # the word fits into the line so just add it
                else:
                    if (line_chars > 0) and had_space:
                        text_buffer.append(" ")
                        line_chars += 1

                    text_buffer.extend(word)
                    line_chars += len(word)

            # we do none of that fancy crap
            # just add the word to the line
            else:
                if (line_chars > 0) and had_space:
                    text_buffer.append(" ")
                    line_chars += 1

                text_buffer.extend(word)
                line_chars += len(word)

            # possible space was used
            had_space = False

            i = match.end(0)
            continue

        # this should only be reached in case of malformed
        # HTML, but we're permissive and just ignore the
        # current character and keep going
        i += 1

    # make sure the text doesn't end in a newline
    count = cntlbs(text_buffer)
    if count > 0:
        del text_buffer[-count:]

    return text_buffer.tounicode()

# match a quoted sequence inside a quoted part of
# a string. this pattern determines whether that
# section is treated as a part of the quoted string
# of wheter the quoting is split
RE_QTWORD = re.compile("\"\w+(?:\s\w+){1}\"(?!$)")

RE_HEADING    = re.compile("\n(([^\n]+\n)+)\n")
RE_PARAGRAPH  = re.compile("(([^\n]+\n)+)\n")
RE_VALID_HEAD = re.compile("^[^<>\".?!]+$", re.I)
RE_WORD_CONT  = re.compile("(?<=[^\s])-\n(?=[^\s])")

# this function sort of excepts the input text to
# be generated by h2t()
def t2h(text, human_readable=False, em_quotes=True, \
    fix_cont=False, line_width=80):
    # encapsulate text in tag optionally adding
    # <em> tags around quoted text
    def encaps(buf, text, tag):
        # beginnning tag
        buf.extend("<%s>" % (tag))

        # FIXME: it may not be possible to entirely fix this
        # as it's impossible for us to differentiate from proper
        # writing without some kind of heuristic. as of now
        # this WILL fuck up certain types of text formatting.
        if fix_cont:
            text = RE_WORD_CONT.sub("", text)

        # replace all whitespace with regular space
        text = RE_SPACE.sub(" ", text)

        # trim whitespace around text
        i = 0
        while text[i].isspace():
            i += 1
        j = len(text)
        while text[j - 1].isspace():
            j -= 1

        quot = False

        last_space  = 0
        line_length = 2 + len(tag)

        # go through text
        while i < j:
            if em_quotes and (text[i] == "\""):
                if quot:
                    buf.extend("\"</em>")
                    line_length += 6
                    quot = False
                else:
                    buf.extend("<em>\"")
                    line_length += 5
                    quot = True
            else:
                # encode special entities
                if text[i] in HTML_SPECIAL_ENTITIES:
                    seq = "&{};".format(HTML_SPECIAL_ENTITY_SEQS[text[i]])
                    buf.extend(seq)
                    line_length += len(seq)
                # append character
                else:
                    buf.append(text[i])
                    line_length += 1

            # split long lines
            if human_readable:
                if line_length > line_width:
                    buf[last_space] = "\n"
                    line_length = len(buf) - last_space
                elif text[i].isspace():
                    last_space = len(buf) - 1

            i += 1

        # fix quote mismatch but don't add a quote as we can't be
        # sure where the terminating quote is supposed to be
        if quot:
            buf.extend("</em>")
            line_length += 4

        # terminating tag
        buf.extend("</%s>" % (tag))
        line_length += 3 + len(tag)

        if human_readable:
            # need to line split here as well
            if line_length > line_width:
                buf[last_space] = "\n"
            buf.append("\n")

    html_buffer = array.array("u")

    num_heads = 0
    num_pgrps = 0

    start = i = 0
    while i < len(text):

        match = RE_HEADING.match(text, i)
        if match:
            encaps(html_buffer, match.group(1), "h4")
            num_heads += 1
            i = match.end(0)
            continue

        match = RE_PARAGRAPH.match(text, i)
        if match:
            group = match.group(1)
            # special case where the first paragraph will be treated as 
            # a heading if the following conditions are met:
            # 1. the paragraph is less than 140 characters long
            # 2. the paragraph only contains one sentence (no periods)
            # 3. the paragraph doesn't contain quotes
            if (num_heads == 0) and (num_pgrps == 0) and \
                (len(group) < 140) and RE_VALID_HEAD.match(group):
                encaps(html_buffer, group, "h2")
                num_heads += 1
            else:
                encaps(html_buffer, group, "p")
                num_pgrps += 1
            i = match.end(0)
            start = i
            continue

        i += 1

    # is the following necessary?
    if start >= i:
        return html_buffer.tounicode()

    # the following code handles the last paragraph
    # in the case that it isn't terminated by two newlines
    # and just ends at the end of the file
    while (start < len(text)) and text[start].isspace():
        start += 1

    end = len(text) - 1
    while (end > 0) and text[end].isspace():
        end -= 1

    # if there was something other than whitespace
    # at the end, output that as a paragraph
    if start < i:
        encaps(html_buffer, text[start:end + 1], "p")

    return html_buffer.tounicode()
