from sys import exit
from functools import reduce
from itertools import groupby, chain
from collections.abc import Iterable
from io import StringIO, BytesIO
from datetime import datetime
import html
import regex as re
from lxml import etree
import unicodedata
from .wordmatcher import best_match
punct = list(',..·;;·.§')
sent_punct = list('.·;;·.§')
bracket_symbols = '[‹›〚〛#\(\)\|❛❜¯\_\\\/\^\~¯↕→←\{\}\[\]\'｣｢”§°]+'

xml_atts = lambda x: (' ' if len(x) else '')+(' '.join([f'{k.split("}")[-1]}="{v}"' for k, v in x.items()]))
no_ns_atts = lambda x: {k.split('}')[-1]: v for k, v in x.items()}
flat_dict = lambda x: '|'.join([f'{k.split("}")[-1]}={v}' for k, v in x.items()])
compose_inner_func = lambda f, g: lambda x : g(f(x))
composer = lambda *x: reduce(compose_inner_func, x, lambda x: x)
add_ud = lambda x: ''.join([y+'̣' if unicodedata.normalize('NFD', y)[0].lower().isalpha() else y for y in x])
transpose = lambda m: m and [list(map(lambda r: r[0], m)), *transpose(list(filter(len, map(lambda r: r[1:], m))))]
flat_list = lambda x: [item for sublist in x for item in sublist]
plain = lambda x: ''.join([unicodedata.normalize('NFD', a)[0].lower() for a in x if a.isalpha() or a in punct+['_']])
parse_flat_atts = lambda a: {x.split('=')[0]:x.split('=')[1] for  x in a.split('|')}
transform_error = lambda f, el, atts, where: exit(f'Error in {where}: no bracket for {el}, {atts}\n {f}')

def get_edition_xml_str(xml_dom):
    try:
        return etree.tostring(xml_dom.xpath(".//div[@type='edition']")[0]).decode()
    except:
        return ''

def get_match(x, r): 
    if match := re.search(r, x): return match.group(0)
    
def flatten_deep(xs):
    for x in xs:
        if isinstance(x, Iterable) and not isinstance(x, (str, bytes)):
            yield from flatten_deep(x)
        else:
            yield x

def re_recursive(p, r, s):
    while 1: 
        s, n = re.subn(p, r, s)
        if not n: return s

def remove_ns(xml_dom):
    query = "descendant-or-self::*[namespace-uri()!='']"
    for element in xml_dom.xpath(query): element.tag = etree.QName(element).localname
    etree.cleanup_namespaces(xml_dom)
    return xml_dom

def stringify_children(node):
    return ''.join(
        chunk for chunk in chain(
            (node.text,),
            chain(*((etree.tostring(child, with_tail=False, encoding='utf-8').decode(), child.tail) for child in node.getchildren())),
            (node.tail,)) if chunk)

def create_versions(data):
    state = data['state']
    def remove_symbols(t):
        replacements = [
            ('⸦[^⸧]+⸧', ''), 
            ('༺[^༻]+༻', ''), 
            ('⧙[^⧘]+⧘', '|'), 
            ('⦓[^⦔]+⦔', ''), 
            ('⧛[^⧚]+⧚', ''), 
        ]
        for x, y in replacements: t = re.sub(x, y, t)
        return t

    def pre_plain(t):
        t = re.sub('\$(?=.+)', '', t)
        t = re.sub('\*(.*?)\*', '', t)
        return re.sub('(｢[^｣]+｣)+', '_', t)

    def remove_gap_marker(t):
        return re.sub('[｢｣]+', '', t)

    def version_type(k):
        if not ('reg' in k or 'scribaldel' in k or 'corr' in k or 'rdg' in k):
            return 'orig'
        elif not ('rdg' in k or 'scribaldel' in k):
            return 'reg'
        return 'var'

    def get_flag_and_info(t):
        nonlocal state
        flag = ''
        info = []
        app_type = []
        paths = re.findall('(>[a-z]+\[[0-9]+\])', t)
        for p in paths:
            el_id = int(re.search('[0-9]+', p).group(0))
            atts = state['elements'][el_id].attrib
            if app_t:= atts.get('type', ''): app_type.append(app_t)
            if app_type:
                if atts.get('cert', ''): app_type[-1] += '?'
            if resp:= atts.get('resp', ''): info.append(resp)

            if 'reg' in p:
                flag += 'r'
            elif 'orig' in p:
                flag += 'o'
            elif 'corr' in p:
                flag += 'c'
            elif 'sic' in p:
                flag += 's'
            elif 'scribaladd' in p:
                flag += 'a'
            elif 'scribaldel' in p:
                flag += 'd'
            elif 'lem' in p:
                flag += 'l'
            elif 'rdg' in p:
                flag += 'g'
        
        t = re.sub('(>[a-z]+\[[0-9]+\])+', '', t)
        return t, flag, '/'.join(app_type), '/'.join(info)

    def get_lang(t, lang):
        if '⸦' in t: return re.search('(?<=⸦).+?(?=⸧)', t).group(0)
        return lang
    
    def get_hand(t, hand, aow_n):
        if '༺' in t: # handshift
            hand_prev = hand
            hand = re.findall('(?<=༺).+?(?=༻)', t)[-1]
            if hand_prev != hand:
                aow_n += 1
        return hand, aow_n

    def get_line(t, line, line_rend):
        if '⧙' in t: # word-medial lb
            line, line_rend = re.search('(?<=⧙).+?(?=⧘)', t).group(0).split('⟩')
        return line, line_rend
    
    def get_num(t, num, num_rend):
        if '⦓' in t: # num
            if '✓' in t:
                num_rend = 'tick'
                t.replace('✓', '')
            num = re.search('(?<=⦓).+?(?=⦔)', t).group(0)
        return num, num_rend

    lang ='grc'
    document_lang = 'grc'
    prev_n = 1
    n = 1
    hand = 'm1'
    aow_n = 1
    sentence_n = 1
    textpart = []
    line = 1
    line_rend = ''
    

    token_data = []
    increment_sentence = 0
    cancel_increment = 0

    for t in data['tokens']:
        info = '' 
        skip = 0
        num = ''
        num_rend = ''
        lang = document_lang

        if increment_sentence:
            sentence_n += 1
            prev_n = n
            n = 1
            increment_sentence = 0
        
        if cancel_increment:
            n = prev_n
            sentence_n -= 1
            cancel_increment = 0

        if type(t) == str:
            if '⸨' in t:
                atts = parse_flat_atts(t[2:-1])
                if 'lang' in atts:
                    document_lang = atts['lang']
                    lang = document_lang
                else:
                    inc_atts = []
                    if 'n' in atts: inc_atts.append(atts['n'])
                    if 'subtype' in atts: inc_atts.append(atts['subtype'])
                    textpart.append('.'.join(inc_atts))
                skip = 1

            elif '⸩' in t:
                try: textpart.pop()
                except: pass
                skip = 1

            elif '⧛' in t:
                line, line_rend = t[1:-1].split('⟩')
                skip = 1

            elif 'figure-' in t:
                info = state['figures'][int(re.search('(?<=figure-)[0-9]+', t).group(0))-1]

            elif 'note-' in t:
                info = state['notes'][int(re.search('(?<=note-)[0-9]+', t).group(0))-1]

            else:
                if any(s in t for s in sent_punct):
                    increment_sentence = 1
                
                if '§' in t:
                    skip = 1

                if '°' in t:
                    cancel_increment = 1
                    skip = 1

                lang = get_lang(t, lang)
                hand, aow_n = get_hand(t, hand, aow_n)
                line, line_rend = get_line(t, line, line_rend)
                num, num_rend = get_num(t, num, num_rend)

            t = remove_symbols(t)

            if not re.sub(bracket_symbols, '', t).replace('$', ''):
                skip = 1

            plain_t = plain(pre_plain(t))
            t = remove_gap_marker(t)
            
            if not skip:
                token_data.append({
                    'orig_form': t,
                    'orig_plain': plain_t,
                    'orig_flag': '',
                    'orig_app_type': '',
                    'orig_num': num,
                    'orig_num_rend': num_rend,
                    'orig_lang': lang,
                    'orig_info': info,
                    'orig_lemma': None,
                    'orig_postag': None,
                    'orig_relation': None,
                    'orig_head': None,
                    'reg_form': t,
                    'reg_plain': plain_t,
                    'reg_flag': '',
                    'reg_app_type': '',
                    'reg_num': num,
                    'reg_num_rend': num_rend,
                    'reg_lang': lang,
                    'reg_info': info,
                    'reg_lemma': None,
                    'reg_postag': None,
                    'reg_relation': None,
                    'reg_head': None,
                    'vars': [],
                    'n': n,
                    'line': line,
                    'line_rend': line_rend,
                    'sentence_n': sentence_n,
                    'hand': hand,
                    'aow_n': aow_n,
                    'textpart': '/'.join(textpart),
                    'artificial': None,
                    'insertion_id': None,
                })
                n += 1

        else:
            for t_word in t:

                info = '' 
                num = ''
                num_rend = ''
                lang = document_lang
                reg_collected = 0
                var_data = []
                orig_data = {}
                reg_data = {}
                have_reg = 0
                have_orig = 0
                prev_n = n

                if increment_sentence:
                    sentence_n += 1
                    n = 1
                    increment_sentence = 0

                for t_version in t_word.split('⧽'):
                    
                    v = version_type(t_version)
                    
                    if v == 'reg':
                        if reg_collected: v = 'var'
                        reg_collected = 1

        
                    t_version, flag, app_type, info = get_flag_and_info(t_version)

                    if '⧛' in t_version:
                        line, line_rend = t_version[1:-1].split('⟩')

                    elif 'figure-' in t_version:
                        info += '/'+state['figures'][int(re.search('(?<=figure-)[0-9]+', t_version).group(0))-1]

                    elif 'note-' in t_version:
                        info += '/'+state['notes'][int(re.search('(?<=note-)[0-9]+', t_version).group(0))-1]

                    else:
                        if any(s in t_version for s in sent_punct) and v in ['orig', 'reg']:
                            increment_sentence = 1

                        lang = get_lang(t_version, lang)
                        if v != 'var':
                            hand, aow_n = get_hand(t_version, hand, aow_n)
                            line, line_rend = get_line(t_version, line, line_rend)
                        num, num_rend = get_num(t_version, num, num_rend)

                    t_version = remove_symbols(t_version)

                    very_plain = re.sub(bracket_symbols, '', t_version)

                    if very_plain:
                        if v == 'reg':
                            have_reg = very_plain
                        elif v == 'orig':
                            have_orig = very_plain
    
                    plain_t_version = plain(pre_plain(t_version))
                    t_version = remove_gap_marker(t_version)
                    
                    if v == 'orig': 
                        orig_data = {
                            'orig_form': t_version,
                            'orig_plain': plain_t_version,
                            'orig_flag': flag,
                            'orig_app_type': app_type,
                            'orig_num': num,
                            'orig_num_rend': num_rend,
                            'orig_lang': lang,
                            'orig_info': info,
                            'orig_lemma': None,
                            'orig_postag': None,
                            'orig_relation': None,
                            'orig_head': None,
                        }
                    elif v == 'reg': 
                        reg_data = {
                            'reg_form': t_version,
                            'reg_plain': plain_t_version,
                            'reg_flag': flag,
                            'reg_app_type': app_type,
                            'reg_num': num,
                            'reg_num_rend': num_rend,
                            'reg_lang': lang,
                            'reg_info': info,
                            'reg_lemma': None,
                            'reg_postag': None,
                            'reg_relation': None,
                            'reg_head': None,
                        }
                    else:
                        var_data.append({
                            'form': t_version,
                            'plain': plain_t_version,
                            'flag': flag,
                            'app_type': app_type,
                            'info': info,
                            'num': num,
                            'num_rend': num_rend,
                            'lang': lang,
                        })
                if not have_reg:
                    have_reg = '$'
                if have_orig and not (have_orig == have_reg == '$'):
                    if not reg_data:
                        reg_data = {k.replace('orig', 'reg'): v for k,v in orig_data.items()}

                    common_data = {
                        'vars': var_data,
                        'n': n,
                        'line': line,
                        'line_rend': line_rend,
                        'sentence_n': sentence_n,
                        'hand': hand,
                        'aow_n': aow_n,
                        'textpart': '/'.join(textpart),
                        'artificial': None,
                        'insertion_id': None,
                    }

                    token_data.append(orig_data | reg_data | common_data)

                    n += 1

        # JA TARKISTA: lausejako-jutut & handShiftit. voi mennä PAHASTI pieleen

    return token_data

def stack_alternatives(data):
    def move_brackets(ts):
        for i, t in enumerate(ts):
            t_parts = t.split('⧽')
            for k, part in enumerate(t_parts):
                t_parts[k] = re.sub('(^[^>]+)((>[a-zA-Z]+\[[0-9]+\])+)', r'\2\1', part)
            ts[i] = '⧽'.join(t_parts)
        
        return ts

    def group_recu(ts, rg):
        res = []
        for i, j in groupby(ts, lambda x: get_match(x, rg)):
            k = list(j)
            if not i: 
                res += k
                continue
            res += [group_recu(k, re.escape(i)+'.*?(>([a-z]+)\[[0-9]+\])')]
        return res

    def stack_variants(x):

        my = []
        if type(x) == str:
            return x
        else: 

            if len(x) > 1 and all([type(y) == list for y in x]) and all([type(y) == str for y in flat_list(x)]):
                n = best_match(x, clean_regex="(>[a-z]+\[[0-9]+\])+")
                return ['⧽'.join([re.sub('>(app|choice|subst)\[[0-9]+\]', '', x) for x in l]) for l in zip(*n)]

            elif any([type(y) == list for y in x]) and all([type(y) == str for y in flat_list(x)]):
                return list(flatten_deep(x))

            for i in x:
                my += [stack_variants(i)]
        return my

    grouped_tokens = group_recu(data['tokens'], '(?<!>)>(app|choice|subst)\[[0-9]+\]')
    for i, g in enumerate(grouped_tokens):
        if type(g) == list:
            old_g = []
            c = 0
            while g != old_g:
                old_g = g
                g = stack_variants(g)
                c+= 1
            grouped_tokens[i] = move_brackets(g)
    
    return {'tokens': grouped_tokens, 'state': data['state']}  

def transform_brackets(data):
    element_stack = data['state']['elements']
    tokens = data['tokens']
    cur_file = data['state']['file']

    def bracket_grammar(tag, atts):
        match tag:
            case 'supplied':
                match atts.get('reason', ''):
                    case 'omitted':
                        return ['‹', '›']
                    case 'lost':
                        if atts.get('evidence', '') == 'parallel':
                            return ['_[', ']_']
                        else: return ['[', ']']
                    case 'undefined':
                        return ['|_', '_|']
            case 'surplus':
                return ['{', '}']
            case 'del':
                match atts.get('rend', ''):
                    case 'slashes':
                        s = '/'
                    case 'cross-strokes':
                        s = '✕'
                    case _:
                        s = ''
                return [f'〚{s}', '〛']

            case 'add':
                match atts.get('place', ''):
                    case 'above':
                        return ['\\', '/']
                    case 'below'|'bottom':
                        return ['/', '\\']
                    case 'left':
                        return ['||←:', '||']
                    case 'right':
                        return ['||→:', '||']
                    case 'interlinear':
                        return ['||↕:', '||']
                    case 'margin':
                        if atts.get('rend', '') == 'underline':
                            return ['‹_', '_›']
                        elif atts.get('rend', '') == 'sling':
                            return ['‹|', '|›']

            case 'hi':
                match atts.get('rend', ''):
                    case 'supraline':
                        return ['¯', '¯']
                    case 'tall':
                        return ['~||', '||~']
                    case 'superscript':
                        return ['|^', '^|']
                    case 'above':
                        return ['|^', '^|']
                    case 'subscript':
                        return ['\|', '|/']
                    case 'supraline-underline':
                        return ['¯_', '_¯']
                    case 'underline'|'underlined':
                        return ['_', '_']
                    case 'diaeresis':
                        return ['', '(¨)']
                    case 'acute':
                        return ['', '(´)']
                    case 'asper':
                        return ['', '(῾)']
                    case 'grave':
                        return ['', '(`)']
                    case 'circumflex':
                        return ['', '(^)']
                    case 'lenis':
                        return ['', '(᾿)']
                    case _:
                        return ['', '']
                    
            case 'q':
                return ['❛', '❜']
            case 'expan':
                return ['(', ')']
            case 'ex':
                return ['(', ')']
            case 'abbr':
                return ['(|', '|)']
            case 'num':
                val = atts.get('value', '?') or '?'
                tick = '✓' if atts.get('rend', '') == 'tick' else ''
                return ['', f'⦓{val}{tick}⦔']

            case 'foreign':
                l = atts.get('lang', '')
                return ['', f'⸦{l}⸧']

            case 'l'|'lb':
                brackets = ['⧙', '⧘'] if atts.get('break', '') == 'no' else ['⧛', '⧚']
                return [f' {brackets[0]}{atts.get("n", "?")}⟩{atts.get("rend", "")}{brackets[1]} ', '']

            case 'unclear'|'lg'|'seg':
                return ['', '']
            

    def bracket_transformer(br, stack_id):
        nonlocal element_stack
        m = 0 if br == '>' else 1
        el = element_stack[stack_id]
        tag = el.tag
        atts = no_ns_atts(el.attrib)
        u = '?' if (m and (atts.get('cert', '') or atts.get('precision'))) else ''

        if tags := bracket_grammar(tag, atts):
            return u+tags[m]

        elif tag in ['app', 'choice', 'subst', 'lem', 'rdg', 'reg', 'orig', 'scribaladd', 'scribaldel', 'corr', 'sic']:
            if not m:
                return f'{br}{tag}[{stack_id}]'
            return ''
        else:
            transform_error(cur_file, tag, atts, 'bracket_transform')

    transformed_tokens = [
        re.sub('([><])\[([0-9]+)\]', lambda x: bracket_transformer(x.group(1), int(x.group(2))), t)
        for t in tokens
    ]

    # We need to stringify again, then split again...
    tokens_str = ' '.join(transformed_tokens)
    tokens_str = re.sub(r'\s+', ' ', tokens_str).strip()

    return {'tokens': tokens_str.split(), 'state': data['state']}

def bracketify_words(data):
    tokens = data['tokens']

    for i, t in enumerate(tokens):
        req_closings = []
        brackets = r'([><]\[[0-9]+\])'
        ms = re.findall(brackets, t)
        for s in ms:
            if s[0]=='>':
                req_closings.insert(0, '<'+s[1:])
            elif s[0]=='<':
                if req_closings[0] == s:
                    req_closings = req_closings[1:]
        
        if req_closings:
            tokens[i] += ''.join(req_closings)
            req_openings = []
            for r in req_closings:
                req_openings.insert(0, '>'+r[1:])
            if req_openings:
                try:tokens[i+1] = ''.join(req_openings) + tokens[i+1]
                except: pass

    return {'tokens': tokens, 'state': data['state']}

def normalize_and_split(data):

    xml_str = data['text']
    xml_str = re_recursive(f'(>\[[0-9]+\])(\s+)', r'\2\1', xml_str) # moving spaces
    xml_str = re_recursive(f'(\s+)(<\[[0-9]+\])', r'\2\1', xml_str) # moving spaces
    xml_str = re.sub('\s+⧙', '⧙', xml_str) # non-breaking lbs
    xml_str = re.sub('⧘\s+', '⧘', xml_str) # non-breaking lbs
    xml_str = re.sub(r'([κτχθ])([αεοηωυ]*)([ἀἐἠἰὀὐὠᾀᾐᾠῤἄἔἤἴὄὔὤᾄᾔᾤἂἒἢἲὂὒὢᾂᾒᾢἆἦἶὖὦᾆᾖᾦ])', r'\1ʼ \2\3', xml_str)
    xml_str = re.sub(r'\s+', ' ', xml_str).strip() # normalize spaces
    xml_str = re.sub(r' (⸩.*?⸨[^ ]+)(⧙[^ ]+)', r'\2 \1', xml_str) # bugfix
    xml_str = re.sub(r'(⧙[^⧘]+?⧘)(⧛)', ' \2', xml_str) # bugfix 2
    return {'tokens': xml_str.split(), 'state': data['state']}

def transform_tags(data):
    root = data['root']
    state = data['state']
    things_i = 0
    things_l = []
    unclear = 0

    def symbolify(el, unclear):    
        nonlocal state
        tag = el.tag
        atts = no_ns_atts(el.attrib)
        match tag:
            case 'lb'|'l':
                brackets = ['⧙', '⧘'] if atts.get('break', '') == 'no' else ['⧛', '⧚']
                return f' {brackets[0]}{atts.get("n", "?")}⟩{atts.get("rend", "")}{brackets[1]} '
            
            case 'milestone':
                rend = atts.get('rend', '')
                match rend:
                    case 'paragraphos': return '----'
                    case 'horizontal-rule': return '--------'
                    case 'wavy-line': return '~~~~~~~~'
                    case 'box': return '###'
                    case 'coronis': return '-$$-'
                    case 'diple-obelismene': return '›---'
                    case _: return '*milestone*'
            
            case 'g':
                g_type = atts.get('type', '')
                u = '(?)' if unclear else ''
                match g_type:
                    case 'x': return f'*✕{u}*'
                    case 'slanting-stroke': return f'*╱{u}*'
                    case 'extension': return f'*—{u}*'
                    case 'apostrophe': return f'*⸍{u}*'
                    case 'stauros': return f'*†{u}*'
                    case 'rho-cross': return f'*⳨{u}*'
                    case 'chirho': return f'*☧{u}*'
                    case 'check': return f'*✓{u}*'
                    case 'middot': return f'*‧{u}*'
                    case 'dipunct': return f'*∶{u}*'
                    case 'long-vertical-bar': return f'*｜{u}*'
                    case 'diastole': return f'*⸒{u}*'
                    case 'dot': return f'*•{u}*'
                    case 'unintelligible': return f'*?{u}*'
                    case _: return f"*{g_type}{u}*"
            
            case 'gap'|'space':
                pd = '․'
                brackets = {
                    'lost': ['[', ']'],
                    'omitted': ['‹', '›'],
                    'illegible': ['', ''],
                    'ellipsis': ['', ''],
                    'none': ['', ''],
                }
                units = {
                    'character': '',
                    'line': 'lin',
                    'column': 'col',
                    'none': '',
                }

                sp = 'vac' if tag == 'space' else '_'
                desc = atts.pop('desc', '').lower()
                if desc:
                    if desc == 'non transcribed': desc = f'{pd}nontr'
                    elif desc == 'vestiges': desc = f'{pd}vestig'
                    else: desc = f'{pd}{desc}'
                
                ell = f'{pd}ell' if atts.get('reason', '') == 'ellipsis' else ''

                ca = f'{pd}ca' if atts.get('precision', '') else ''

                quantity = atts.pop('quantity', '')
                atleast = atts.pop('atLeast', '')
                atmost = atts.pop('atMost', '')

                ext = '?' if atts.get('extent', '') else ''
                if quantity:
                    ext = quantity
                elif atleast and atmost:
                    ext = atleast+"-"+atmost
                
                br = brackets[atts.get('reason', 'none')]
                
                prec = '?' if (atts.get('precision', '') or atts.get('cert', '')) else ''
                unit = units[atts.get('unit', 'none')]

                return f'｢{br[0]}{sp}{desc}{ell}{ca}{pd}{ext}{unit}{prec}{br[1]}｣'

            case 'note':
                note = []
                note_text = atts.pop('text', '')
                note_ref = atts.pop('ref_n', '')
                note_ref_text = atts.pop('ref_text', '')
                if note_text: note.append(note_text)
                if note_ref: note.append(f'(ref:{note_ref})')
                if note_ref_text: note.append(f'({note_ref_text})')
                state['notes'].append(' '.join(note))
                return f' note-{len(state["notes"])} '

            case 'figure':
                state['figures'].append(atts.get('desc', '')+('(?)' if atts.get('cert', '') else ''))
                return f' figure-{len(state["figures"])} '

            case 'handShift':
                n = atts.pop('new', '')
                u = '?' if atts.get('cert', '') else ''
                return  f'༺{n}{u}༻'

            case 'supplied'|'unclear'|'ab'|'del'|'div'|'hi'|'add'|'q'|'w'|'r':
                return ''
            

            case _:
                transform_error(state['file'], tag, atts, 'symbolize')

    def pre_tokenize(s, unclear):
        s = add_ud(s) if unclear else s
        return re.sub(f'([{"".join(punct)}])', r' \1 ', s)

    def transformer(root):
        nonlocal things_i
        nonlocal things_l
        nonlocal unclear
        nonlocal state
        transf = ''
        unclear = 1 if root.tag == 'unclear' else unclear
        if not (len(root) or root.text): # <el/>
            transf += symbolify(root, unclear)
        else:
            if root.tag == 'div':
                atts = no_ns_atts(root.attrib)
                l = root.attrib.pop('lang', '')
                atts.pop('space', '')
                atts.pop('type', '')
                if l: transf += f' ⸨[lang={l}] '
                else: transf += f' ⸨[{flat_dict(atts)}] '
            else:
                state['elements'].append(root)
                things_l.append(things_i)

                sep = ' ' if root.tag in ['app', 'choice', 'subst'] else ''
                
                transf += f'{sep}>[{things_i}]'
                things_i += 1
            if root.text: 
                transf += pre_tokenize(root.text, unclear)
                
            if len(root):
                for elem in root:
                    transf += transformer(elem)
                    if elem.tail:
                        transf += pre_tokenize(elem.tail, unclear)
            
            if root.tag == 'div':
                transf += ' ⸩ '
            else:
                transf += f'<[{things_l[-1]}]'
                things_l.pop()
        unclear = 0 if root.tag == 'unclear' else unclear
        return transf

    transformed = transformer(root)
    return {'text': transformed, 'state': state}

def preformat(data):
    xml_str = data['text']
    replacements = [
        ('<[\/]?ab>', ''), 
        ('<num[^>]*\/>', ''),
        (r'\s+<\/ex>', '-?</ex>'), 
        ('</lem>([\s\S]*?)<rdg', '</lem><rdg'),
        (' break=\"no\"/><choice', '/><choice'),
        (' break=\"no\"/><app', '/><app'),
        ('(<\/(lem|rdg|sic|corr|orig|reg|subst|choice|app)>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'),  
        ('(<(lem|rdg|sic|corr|orig|reg|subst|choice|app)[^>]*\/>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'), 
        ('(<\/(lem|rdg|sic|corr|orig|reg|subst|choice|app)[^>]*>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'), 
        ('<\/add><del', '</add> <del'), 
        ('(<add[^>]*>)(<del)', r'\1 \2'),
        ('(<add[^>]*>)(<del)', r'\1 \2'),
        ('(<lb[^>]+>)', r' \1 '),
        ('(\s*)(<lb[^\/]+break=\"no\"[^\/]*\/>)(\s*)', r'\2'),
    ]

    for x, y in replacements: xml_str = re.sub(x, y,xml_str)

    root = etree.parse(StringIO(xml_str or '<r/>')).getroot()

    for bad in root.xpath(".//p|.//locus|.//div[@type='bibliography']|.//div[@type='commentary']"):
        bad.getparent().remove(bad)

    for x in root.xpath('.//app|.//certainty|.//desc|.//figDesc|.//ref|.//g|.//note|.//del[@rend="corrected"]|.//add[@place="inline"]|num|.//reg[@xml:lang]'):
        match x.tag:
            case 'app':
                app_type = x.attrib.get('type', '')
                for c in x.getchildren():
                    if c.tag in ['lem', 'rdg']:
                        c.attrib['type'] = app_type
            case 'certainty':
                x.getparent().attrib['cert'] = 'low'
                x.getparent().remove(x)
            case 'desc'|'figDesc':
                x.getparent().attrib['desc'] = x.text
                x.getparent().remove(x)
            case 'ref':
                x.getparent().attrib['ref_n'] = x.attrib.get('n', '')
                x.getparent().attrib['ref_text'] = x.text
                x.getparent().remove(x)
            case 'g':
                x.text = ''
            case 'note':
                if len(x):
                    x.attrib['text'] = stringify_children(x).strip()
                    for c in x.getchildren():
                        if not c.tag == 'ref':
                            x.remove(c)
                else:
                    x.attrib['text'] = x.text or ''
                x.text = ''

            case 'del':
                x.tag = 'scribaldel'
                x.attrib.clear()
            case 'add':
                x.tag = 'scribaladd'
                x.attrib.clear()
            #case 'reg':
            #    x.tag = 'langreg'
            #    x.attrib.clear()=

    for x in root.xpath('.//lem|.//rdg|.//scribaladd|.//scribaldel|.//reg|.//orig|.//sic|.//corr'):
        if not ''.join(x.itertext()):
            x.text = '$'

    # Remove comments
    comments = root.xpath('//comment()')
    for c in comments:
        p = c.getparent()
        p.remove(c)

    
    # The below code does some additional preformatting
    # re: the variation containers <subst>, <app>, and <choice>

    # First, to string again
    xml_str = etree.tostring(root, encoding='utf-8').decode()


    def move_subst_prefix(x):
        """ Moves [prefix]<subst> to <subst><add>[prefix]"""
        space = x.group(1)
        prefix = x.group(2)
        subst = x.group(4)

        # Only move if prefix is valid XML
        try:
            _ = etree.fromstring(('<r>'+prefix+'</r>').encode('ascii', 'xmlcharrefreplace'))
            return space+subst+prefix
        
        # Otherwise, return as is
        except:
            return space+prefix+subst

    def move_choice_suffix(x):
        """ Moves <choice>[suffix] to [suffix]</orig> and [suffix]</reg>"""
        return re.sub('(</(orig|reg)>)', x.group(3)+r'\1', x.group(1))

    # ---- SUBST ----

    # Move [prefix]<subst> to subst
    xml_str = re.sub('(\s+)((<[^/>]+/?>|[a-zA-Z0-9]|</[a-z]+>|\p{Greek})+)(<subst><scribaladd>)', lambda x: move_subst_prefix(x), xml_str)

    # Move [prefix]<subst> to subst (for multi-word <add>s) 
    xml_str = re.sub('(<add place=\"above\">[^>]+)\s([^>]+</add>)(<subst><scribaladd>)', r'\1</add> \3<add place="above">\2', xml_str)
    
    # Finally, isolate <subst>
    xml_str = re.sub('(?<!\s)<subst>', ' <subst>', xml_str)
    xml_str = re.sub('</subst>(?!\s)', '</subst> ', xml_str)

    # ---- CHOICE ----

    # Move <expan> or <abbr> suffix to <choice>
    xml_str = re.sub('(<choice>(?:(?!(<choice|</choice>)).)*</choice>)(<expan>.*?</expan>)', lambda x: move_choice_suffix(x), xml_str)
    xml_str = re.sub('(<choice>(?:(?!(<choice|</choice>)).)*</choice>)(<abbr>.*?</abbr>)', lambda x: move_choice_suffix(x), xml_str)
    xml_str = re.sub('(<choice>(?:(?!(<choice|</choice>)).)*</choice>)(<gap[^>]*?>)', lambda x: move_choice_suffix(x), xml_str)
    # Isolate <choice>
    xml_str = re.sub('(?<!\s)<choice>', ' <choice>', xml_str)
    xml_str = re.sub('</choice>(?!\s)', '</choice> ', xml_str)

    # ---- APP ----

    # Isolate <app>
    xml_str = re.sub('(?<!\s)<app', ' <app', xml_str)
    xml_str = re.sub('</app>(?!\s)', '</app> ', xml_str)

    # ---- dots are gaps ----

    xml_str = re.sub('\.{2,}', '<gap reason="illegible" extent="unknown" unit="character"/>', xml_str)

    # Ensure space after ’
    xml_str = re.sub('’(?!\s)', '’ ', xml_str)
    xml_str = re.sub('ʼ(?!\s)', 'ʼ ', xml_str)

    # To tree again
    root = etree.parse(BytesIO(xml_str.encode('ascii', 'xmlcharrefreplace'))).getroot()
 
    return {'root': root, 'state': data['state']}

def get_meta(xml_dom, path):
    series_name = 'Unknown series'
    if series_path_fragment := re.findall('(DDB_EpiDoc_XML|DCLP)\/(.*?)(?=\/)', path):
        series_name = series_path_fragment[-1][-1] if len(series_path_fragment[0]) > 1 else series_path_fragment[-1]
    
    return {
        'series_name': series_name,
        'name': path.split('/')[-1],
        'hgv': ', '.join((xml_dom.xpath('.//idno[@type="HGV"]/text()') or [''])[0].split(' ')),
        'tm': ', '.join((xml_dom.xpath('.//idno[@type="TM"]/text()') or [''])[0].split(' ')),
        'last_change': datetime.strptime((xml_dom.xpath('.//change/@when') or ['9999-12-31'])[0][:10], '%Y-%m-%d')
    }

def init_state(xml_file=None):
    return {
        'elements': [],
        'notes': [],
        'figures': [],
        'file': xml_file,
    }

def init_tokenizer(xml_str, state):
    return composer(
        preformat,
        transform_tags,
        normalize_and_split,
        bracketify_words,
        transform_brackets,
        stack_alternatives,
        create_versions
    )({'text': xml_str, 'state': state})


def tokenize_file(xml_file):
    xml_dom = remove_ns(etree.parse(xml_file))
    meta = get_meta(xml_dom, xml_file)
    xml_str = get_edition_xml_str(xml_dom)
    state = init_state(xml_file)

    return {
        'meta': meta,
        'edition_xml': html.unescape(xml_str),
        'tokens': lambda: init_tokenizer(xml_str, state)
    }

def tokenize_string(xml_str):
    state = init_state()

    return {
        'edition_xml': xml_str,
        'tokens': lambda: init_tokenizer(xml_str.encode('ascii', 'xmlcharrefreplace').decode(), state)
    }













def do_the_test(data):
    rep_stack = []

    def mask(x):
        nonlocal rep_stack
        ret = f'❨{len(rep_stack)}❩'
        rep_stack.append(x.group(0))
        return ret
    def unmask(x):
        return rep_stack[int(x.group(1))]

    xml_str = etree.tostring(data['root'], encoding='utf-8').decode()

    matches = re.findall('</reg>([\s\S]*?)<orig', xml_str)
    for m in matches:
        m = m.strip()
        if m:
            if not m.startswith('<reg'):
                print(data['state']['file'])
                print(m)

    return 0,0,0

    #xml_str = re.sub('(?i)<(?!choice|app|subst|/choice|/app|/subst).*?>', lambda x: mask(x), xml_str)
    xml_str = re.sub('(?i)<(?!app|/app).*?>', lambda x: mask(x), xml_str)



    # PREFIXES

    '''


    fix = re.finditer('\s+(\p{Greek}|[a-zA-Z❨❩0-9])+<(choice)', xml_str)
    fix2 = re.finditer('<choice', xml_str)
    pr = list(fix)
    comp = list(fix2)

    for m in pr:
        match = m.group(0)

        match = re.sub('[\s❨❩0-9<]+', '', match).replace('choice', '')
        if match:
            print(data['state']['file'])
            print(m.group(0))
            morig = re.sub('❨([0-9]+)❩', lambda x: unmask(x), m.group(0))
            
            print(morig)

            morig = ('<root>'+morig[:-6]+'</root>').encode('ascii', 'xmlcharrefreplace')

            try: 
                print(etree.fromstring(morig))
                
            except:
                print('EXCEPTED!')
        #print(xml_str[m.span(0)[0]-30:m.span(0)[0]+30])
        #print(xml_str)
    '''
    
    fix = re.finditer('</app>(\p{Greek}|[a-zA-Z❨❩0-9])+\s+', xml_str)
    #fix = re.finditer('</choice>[\S]+\s', xml_str)
    su = list(fix)

    for m in su:
        match = m.group(0)

        match = re.sub('[\s❨❩0-9]+', '', match).replace('</app>', '')
        if match:
            print(data['state']['file'])
            print(m.group(0))
            morig = re.sub('❨([0-9]+)❩', lambda x: unmask(x), m.group(0))

            #expan = re.search('<expan>.*?</expan>', morig[9:])
            #if e := expan.group(0) if expan else None:
            #
            #    print(e)
            # 
            #    print(morig)
            #

            morig = ('<root>'+morig[6:]+'</root>').encode('ascii', 'xmlcharrefreplace')
            print(morig)
            try: 

                a = etree.fromstring(morig)
                print(a)
                
            except:
                print('COULD NOT PARSE ABOVE\n')
                
            
    return 0, len(su), 0

    

    
    return len(pr),0, len(comp)


def tests(xml, s=0):
    if not s:
        xml_file = xml
        xml_dom = remove_ns(etree.parse(xml))
        xml_str = get_edition_xml_str(xml_dom)
    else: 
        xml_str = xml
        xml_file = 'No file'
    state = init_state(xml_file)

    return composer(
        preformat,
        do_the_test
    )({'text': xml_str, 'state': state})

'''
def tests(xml, s=0):
    if not s:
        xml_file = xml
        xml_dom = remove_ns(etree.parse(xml))
        xml_str = get_edition_xml_str(xml_dom)
    else: 
        xml_str = xml
        xml_file = 'No file'

    rep_stack = []

    def mask(x):
        nonlocal rep_stack
        
        ret = f'❨{len(rep_stack)}❩'
        #ret = ''
        rep_stack.append(x.group(0))
        return ret

    def unmask(x):
        return rep_stack[int(x.group(1))]
    
    xml_str = html.unescape(xml_str)
    
    xml_str = re.sub('\s*<lb[^\/]+break="no"[^\/]*\/>', '', xml_str)

    replacements = [
        ('<[\/]?ab>', ''), 
        ('<num[^>]*\/>', ''),
        (r'\s+<\/ex>', '-?</ex>'), 
        ('(<\/(lem|rdg|sic|corr|orig|reg|subst|choice|app)>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'),  
        ('(<(lem|rdg|sic|corr|orig|reg|subst|choice|app)[^>]*\/>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'), 
        ('(<\/(lem|rdg|sic|corr|orig|reg|subst|choice|app)[^>]*>)(<(lem|rdg|sic|corr|orig|reg|subst|choice|app))', r'\1 \3'), 
        ('<\/add><del', '</add> <del'), 
        ('(<add[^>]*>)(<del)', r'\1 \2'),
    ]

    for x, y in replacements: xml_str = re.sub(x, y,xml_str)
    
    xml_str = re.sub('(?i)<(?!choice|app|subst|/choice|/app|/subst).*?>', lambda x: mask(x), xml_str)
    
    print(xml_str)
    #xml_str = re.sub('❨([0-9]+)❩', lambda x: unmask(x), xml_str)

    #fix = re.finditer('\s+(\p{Greek}|[a-zA-Z])+<(choice|app|subst)', xml_str)
    fix = re.finditer('\s+(\p{Greek}|[a-zA-Z])+<(subst)', xml_str)
    fix2 = re.finditer('<subst', xml_str)
    pr = list(fix)
    comp = list(fix2)
    if pr:
        print('\nPREFIXES FOUND\n=======>')
        print(xml_file)
    for m in pr:
        print(f'match: {m.group(0)}\n')
        print(xml_str[m.span(0)[0]-30:m.span(0)[0]+30])
        #print(xml_str)

    fix = re.finditer('</(choice|app|subst)>(\p{Greek}|[a-zA-Z])+\s+', xml_str)
    su = list(fix)
    #if su:
    #    print('\nSUFFIXES FOUND\n<=======')
    #    print(xml_file)
    #for m in su:
    #    print(f'match: {m.group(0)}\n')
    #    print(xml_str[m.span(0)[0]-30:m.span(0)[0]+30])


    return len(pr), len(su), len(comp)
'''