-module(nakai@html).
-compile([no_auto_import, nowarn_unused_vars]).

-export([title/1, a/2, a_text/2, abbr/2, abbr_text/2, address/2, address_text/2, area/1, article/2, article_text/2, aside/2, aside_text/2, audio/2, audio_text/2, b/2, b_text/2, base/1, bdi/2, bdi_text/2, bdo/2, bdo_text/2, blockquote/2, blockquote_text/2, br/1, button/2, button_text/2, canvas/2, canvas_text/2, caption/2, caption_text/2, cite/2, cite_text/2, code/2, code_text/2, col/2, col_text/2, colgroup/2, colgroup_text/2, data/2, data_text/2, datalist/2, datalist_text/2, dd/2, dd_text/2, del/2, del_text/2, details/2, details_text/2, dfn/2, dfn_text/2, dialog/2, dialog_text/2, 'div'/2, div_text/2, dl/2, dl_text/2, dt/2, dt_text/2, em/2, em_text/2, embed/2, embed_text/2, fieldset/2, fieldset_text/2, figcaption/2, figcaption_text/2, figure/2, figure_text/2, footer/2, footer_text/2, form/2, form_text/2, h1/2, h1_text/2, h2/2, h2_text/2, h3/2, h3_text/2, h4/2, h4_text/2, h5/2, h5_text/2, h6/2, h6_text/2, header/2, header_text/2, hr/1, i/2, i_text/2, iframe/2, iframe_text/2, img/1, input/1, ins/2, ins_text/2, kbd/2, kbd_text/2, label/2, label_text/2, legend/2, legend_text/2, li/2, li_text/2, link/1, main/2, main_text/2, map/2, map_text/2, mark/2, mark_text/2, math/2, math_text/2, menu/2, menu_text/2, menuitem/2, menuitem_text/2, meta/1, meter/2, meter_text/2, nav/2, nav_text/2, noscript/2, noscript_text/2, object/2, object_text/2, ol/2, ol_text/2, optgroup/2, optgroup_text/2, option/2, option_text/2, output/2, output_text/2, p/2, p_text/2, param/2, param_text/2, picture/2, picture_text/2, pre/2, pre_text/2, progress/2, progress_text/2, q/2, q_text/2, rp/2, rp_text/2, rt/2, rt_text/2, ruby/2, ruby_text/2, s/2, s_text/2, samp/2, samp_text/2, section/2, section_text/2, select/2, select_text/2, small/2, small_text/2, source/1, span/2, span_text/2, strong/2, strong_text/2, sub/2, sub_text/2, summary/2, summary_text/2, sup/2, sup_text/2, svg/2, svg_text/2, table/2, table_text/2, tbody/2, tbody_text/2, td/2, td_text/2, textarea/2, textarea_text/2, tfoot/2, tfoot_text/2, th/2, th_text/2, thead/2, thead_text/2, time/2, time_text/2, tr/2, tr_text/2, track/1, u/2, u_text/2, ul/2, ul_text/2, var/2, var_text/2, video/2, video_text/2, wbr/2, wbr_text/2]).
-export_type([node_/1]).

-type node_(FSS) :: {doctype, binary()} |
    {html, list(nakai@html@attrs:attr(FSS)), list(node_(FSS))} |
    {head, list(node_(FSS))} |
    {body, list(nakai@html@attrs:attr(FSS)), list(node_(FSS))} |
    {fragment, list(node_(FSS))} |
    {element, binary(), list(nakai@html@attrs:attr(FSS)), list(node_(FSS))} |
    {leaf_element, binary(), list(nakai@html@attrs:attr(FSS))} |
    {comment, binary()} |
    {text, binary()} |
    {unsafe_text, binary()} |
    {script, binary()} |
    nothing.

-spec title(binary()) -> node_(any()).
title(Text) ->
    {element, <<"title"/utf8>>, [], [{text, Text}]}.

-spec a(list(nakai@html@attrs:attr(FSV)), list(node_(FSV))) -> node_(FSV).
a(Attrs, Children) ->
    {element, <<"a"/utf8>>, Attrs, Children}.

-spec a_text(list(nakai@html@attrs:attr(FTB)), binary()) -> node_(FTB).
a_text(Attrs, Text) ->
    {element, <<"a"/utf8>>, Attrs, [{text, Text}]}.

-spec abbr(list(nakai@html@attrs:attr(FTF)), list(node_(FTF))) -> node_(FTF).
abbr(Attrs, Children) ->
    {element, <<"abbr"/utf8>>, Attrs, Children}.

-spec abbr_text(list(nakai@html@attrs:attr(FTL)), binary()) -> node_(FTL).
abbr_text(Attrs, Text) ->
    {element, <<"abbr"/utf8>>, Attrs, [{text, Text}]}.

-spec address(list(nakai@html@attrs:attr(FTP)), list(node_(FTP))) -> node_(FTP).
address(Attrs, Children) ->
    {element, <<"address"/utf8>>, Attrs, Children}.

-spec address_text(list(nakai@html@attrs:attr(FTV)), binary()) -> node_(FTV).
address_text(Attrs, Text) ->
    {element, <<"address"/utf8>>, Attrs, [{text, Text}]}.

-spec area(list(nakai@html@attrs:attr(FTZ))) -> node_(FTZ).
area(Attrs) ->
    {leaf_element, <<"area"/utf8>>, Attrs}.

-spec article(list(nakai@html@attrs:attr(FUD)), list(node_(FUD))) -> node_(FUD).
article(Attrs, Children) ->
    {element, <<"article"/utf8>>, Attrs, Children}.

-spec article_text(list(nakai@html@attrs:attr(FUJ)), binary()) -> node_(FUJ).
article_text(Attrs, Text) ->
    {element, <<"article"/utf8>>, Attrs, [{text, Text}]}.

-spec aside(list(nakai@html@attrs:attr(FUN)), list(node_(FUN))) -> node_(FUN).
aside(Attrs, Children) ->
    {element, <<"aside"/utf8>>, Attrs, Children}.

-spec aside_text(list(nakai@html@attrs:attr(FUT)), binary()) -> node_(FUT).
aside_text(Attrs, Text) ->
    {element, <<"aside"/utf8>>, Attrs, [{text, Text}]}.

-spec audio(list(nakai@html@attrs:attr(FUX)), list(node_(FUX))) -> node_(FUX).
audio(Attrs, Children) ->
    {element, <<"audio"/utf8>>, Attrs, Children}.

-spec audio_text(list(nakai@html@attrs:attr(FVD)), binary()) -> node_(FVD).
audio_text(Attrs, Text) ->
    {element, <<"audio"/utf8>>, Attrs, [{text, Text}]}.

-spec b(list(nakai@html@attrs:attr(FVH)), list(node_(FVH))) -> node_(FVH).
b(Attrs, Children) ->
    {element, <<"b"/utf8>>, Attrs, Children}.

-spec b_text(list(nakai@html@attrs:attr(FVN)), binary()) -> node_(FVN).
b_text(Attrs, Text) ->
    {element, <<"b"/utf8>>, Attrs, [{text, Text}]}.

-spec base(list(nakai@html@attrs:attr(FVR))) -> node_(FVR).
base(Attrs) ->
    {leaf_element, <<"base"/utf8>>, Attrs}.

-spec bdi(list(nakai@html@attrs:attr(FVV)), list(node_(FVV))) -> node_(FVV).
bdi(Attrs, Children) ->
    {element, <<"bdi"/utf8>>, Attrs, Children}.

-spec bdi_text(list(nakai@html@attrs:attr(FWB)), binary()) -> node_(FWB).
bdi_text(Attrs, Text) ->
    {element, <<"bdi"/utf8>>, Attrs, [{text, Text}]}.

-spec bdo(list(nakai@html@attrs:attr(FWF)), list(node_(FWF))) -> node_(FWF).
bdo(Attrs, Children) ->
    {element, <<"bdo"/utf8>>, Attrs, Children}.

-spec bdo_text(list(nakai@html@attrs:attr(FWL)), binary()) -> node_(FWL).
bdo_text(Attrs, Text) ->
    {element, <<"bdo"/utf8>>, Attrs, [{text, Text}]}.

-spec blockquote(list(nakai@html@attrs:attr(FWP)), list(node_(FWP))) -> node_(FWP).
blockquote(Attrs, Children) ->
    {element, <<"blockquote"/utf8>>, Attrs, Children}.

-spec blockquote_text(list(nakai@html@attrs:attr(FWV)), binary()) -> node_(FWV).
blockquote_text(Attrs, Text) ->
    {element, <<"blockquote"/utf8>>, Attrs, [{text, Text}]}.

-spec br(list(nakai@html@attrs:attr(FWZ))) -> node_(FWZ).
br(Attrs) ->
    {leaf_element, <<"br"/utf8>>, Attrs}.

-spec button(list(nakai@html@attrs:attr(FXD)), list(node_(FXD))) -> node_(FXD).
button(Attrs, Children) ->
    {element, <<"button"/utf8>>, Attrs, Children}.

-spec button_text(list(nakai@html@attrs:attr(FXJ)), binary()) -> node_(FXJ).
button_text(Attrs, Text) ->
    {element, <<"button"/utf8>>, Attrs, [{text, Text}]}.

-spec canvas(list(nakai@html@attrs:attr(FXN)), list(node_(FXN))) -> node_(FXN).
canvas(Attrs, Children) ->
    {element, <<"canvas"/utf8>>, Attrs, Children}.

-spec canvas_text(list(nakai@html@attrs:attr(FXT)), binary()) -> node_(FXT).
canvas_text(Attrs, Text) ->
    {element, <<"canvas"/utf8>>, Attrs, [{text, Text}]}.

-spec caption(list(nakai@html@attrs:attr(FXX)), list(node_(FXX))) -> node_(FXX).
caption(Attrs, Children) ->
    {element, <<"caption"/utf8>>, Attrs, Children}.

-spec caption_text(list(nakai@html@attrs:attr(FYD)), binary()) -> node_(FYD).
caption_text(Attrs, Text) ->
    {element, <<"caption"/utf8>>, Attrs, [{text, Text}]}.

-spec cite(list(nakai@html@attrs:attr(FYH)), list(node_(FYH))) -> node_(FYH).
cite(Attrs, Children) ->
    {element, <<"cite"/utf8>>, Attrs, Children}.

-spec cite_text(list(nakai@html@attrs:attr(FYN)), binary()) -> node_(FYN).
cite_text(Attrs, Text) ->
    {element, <<"cite"/utf8>>, Attrs, [{text, Text}]}.

-spec code(list(nakai@html@attrs:attr(FYR)), list(node_(FYR))) -> node_(FYR).
code(Attrs, Children) ->
    {element, <<"code"/utf8>>, Attrs, Children}.

-spec code_text(list(nakai@html@attrs:attr(FYX)), binary()) -> node_(FYX).
code_text(Attrs, Text) ->
    {element, <<"code"/utf8>>, Attrs, [{text, Text}]}.

-spec col(list(nakai@html@attrs:attr(FZB)), list(node_(FZB))) -> node_(FZB).
col(Attrs, Children) ->
    {element, <<"col"/utf8>>, Attrs, Children}.

-spec col_text(list(nakai@html@attrs:attr(FZH)), binary()) -> node_(FZH).
col_text(Attrs, Text) ->
    {element, <<"col"/utf8>>, Attrs, [{text, Text}]}.

-spec colgroup(list(nakai@html@attrs:attr(FZL)), list(node_(FZL))) -> node_(FZL).
colgroup(Attrs, Children) ->
    {element, <<"colgroup"/utf8>>, Attrs, Children}.

-spec colgroup_text(list(nakai@html@attrs:attr(FZR)), binary()) -> node_(FZR).
colgroup_text(Attrs, Text) ->
    {element, <<"colgroup"/utf8>>, Attrs, [{text, Text}]}.

-spec data(list(nakai@html@attrs:attr(FZV)), list(node_(FZV))) -> node_(FZV).
data(Attrs, Children) ->
    {element, <<"data"/utf8>>, Attrs, Children}.

-spec data_text(list(nakai@html@attrs:attr(GAB)), binary()) -> node_(GAB).
data_text(Attrs, Text) ->
    {element, <<"data"/utf8>>, Attrs, [{text, Text}]}.

-spec datalist(list(nakai@html@attrs:attr(GAF)), list(node_(GAF))) -> node_(GAF).
datalist(Attrs, Children) ->
    {element, <<"datalist"/utf8>>, Attrs, Children}.

-spec datalist_text(list(nakai@html@attrs:attr(GAL)), binary()) -> node_(GAL).
datalist_text(Attrs, Text) ->
    {element, <<"datalist"/utf8>>, Attrs, [{text, Text}]}.

-spec dd(list(nakai@html@attrs:attr(GAP)), list(node_(GAP))) -> node_(GAP).
dd(Attrs, Children) ->
    {element, <<"dd"/utf8>>, Attrs, Children}.

-spec dd_text(list(nakai@html@attrs:attr(GAV)), binary()) -> node_(GAV).
dd_text(Attrs, Text) ->
    {element, <<"dd"/utf8>>, Attrs, [{text, Text}]}.

-spec del(list(nakai@html@attrs:attr(GAZ)), list(node_(GAZ))) -> node_(GAZ).
del(Attrs, Children) ->
    {element, <<"del"/utf8>>, Attrs, Children}.

-spec del_text(list(nakai@html@attrs:attr(GBF)), binary()) -> node_(GBF).
del_text(Attrs, Text) ->
    {element, <<"del"/utf8>>, Attrs, [{text, Text}]}.

-spec details(list(nakai@html@attrs:attr(GBJ)), list(node_(GBJ))) -> node_(GBJ).
details(Attrs, Children) ->
    {element, <<"details"/utf8>>, Attrs, Children}.

-spec details_text(list(nakai@html@attrs:attr(GBP)), binary()) -> node_(GBP).
details_text(Attrs, Text) ->
    {element, <<"details"/utf8>>, Attrs, [{text, Text}]}.

-spec dfn(list(nakai@html@attrs:attr(GBT)), list(node_(GBT))) -> node_(GBT).
dfn(Attrs, Children) ->
    {element, <<"dfn"/utf8>>, Attrs, Children}.

-spec dfn_text(list(nakai@html@attrs:attr(GBZ)), binary()) -> node_(GBZ).
dfn_text(Attrs, Text) ->
    {element, <<"dfn"/utf8>>, Attrs, [{text, Text}]}.

-spec dialog(list(nakai@html@attrs:attr(GCD)), list(node_(GCD))) -> node_(GCD).
dialog(Attrs, Children) ->
    {element, <<"dialog"/utf8>>, Attrs, Children}.

-spec dialog_text(list(nakai@html@attrs:attr(GCJ)), binary()) -> node_(GCJ).
dialog_text(Attrs, Text) ->
    {element, <<"dialog"/utf8>>, Attrs, [{text, Text}]}.

-spec 'div'(list(nakai@html@attrs:attr(GCN)), list(node_(GCN))) -> node_(GCN).
'div'(Attrs, Children) ->
    {element, <<"div"/utf8>>, Attrs, Children}.

-spec div_text(list(nakai@html@attrs:attr(GCT)), binary()) -> node_(GCT).
div_text(Attrs, Text) ->
    {element, <<"div"/utf8>>, Attrs, [{text, Text}]}.

-spec dl(list(nakai@html@attrs:attr(GCX)), list(node_(GCX))) -> node_(GCX).
dl(Attrs, Children) ->
    {element, <<"dl"/utf8>>, Attrs, Children}.

-spec dl_text(list(nakai@html@attrs:attr(GDD)), binary()) -> node_(GDD).
dl_text(Attrs, Text) ->
    {element, <<"dl"/utf8>>, Attrs, [{text, Text}]}.

-spec dt(list(nakai@html@attrs:attr(GDH)), list(node_(GDH))) -> node_(GDH).
dt(Attrs, Children) ->
    {element, <<"dt"/utf8>>, Attrs, Children}.

-spec dt_text(list(nakai@html@attrs:attr(GDN)), binary()) -> node_(GDN).
dt_text(Attrs, Text) ->
    {element, <<"dt"/utf8>>, Attrs, [{text, Text}]}.

-spec em(list(nakai@html@attrs:attr(GDR)), list(node_(GDR))) -> node_(GDR).
em(Attrs, Children) ->
    {element, <<"em"/utf8>>, Attrs, Children}.

-spec em_text(list(nakai@html@attrs:attr(GDX)), binary()) -> node_(GDX).
em_text(Attrs, Text) ->
    {element, <<"em"/utf8>>, Attrs, [{text, Text}]}.

-spec embed(list(nakai@html@attrs:attr(GEB)), list(node_(GEB))) -> node_(GEB).
embed(Attrs, Children) ->
    {element, <<"embed"/utf8>>, Attrs, Children}.

-spec embed_text(list(nakai@html@attrs:attr(GEH)), binary()) -> node_(GEH).
embed_text(Attrs, Text) ->
    {element, <<"embed"/utf8>>, Attrs, [{text, Text}]}.

-spec fieldset(list(nakai@html@attrs:attr(GEL)), list(node_(GEL))) -> node_(GEL).
fieldset(Attrs, Children) ->
    {element, <<"fieldset"/utf8>>, Attrs, Children}.

-spec fieldset_text(list(nakai@html@attrs:attr(GER)), binary()) -> node_(GER).
fieldset_text(Attrs, Text) ->
    {element, <<"fieldset"/utf8>>, Attrs, [{text, Text}]}.

-spec figcaption(list(nakai@html@attrs:attr(GEV)), list(node_(GEV))) -> node_(GEV).
figcaption(Attrs, Children) ->
    {element, <<"figcaption"/utf8>>, Attrs, Children}.

-spec figcaption_text(list(nakai@html@attrs:attr(GFB)), binary()) -> node_(GFB).
figcaption_text(Attrs, Text) ->
    {element, <<"figcaption"/utf8>>, Attrs, [{text, Text}]}.

-spec figure(list(nakai@html@attrs:attr(GFF)), list(node_(GFF))) -> node_(GFF).
figure(Attrs, Children) ->
    {element, <<"figure"/utf8>>, Attrs, Children}.

-spec figure_text(list(nakai@html@attrs:attr(GFL)), binary()) -> node_(GFL).
figure_text(Attrs, Text) ->
    {element, <<"figure"/utf8>>, Attrs, [{text, Text}]}.

-spec footer(list(nakai@html@attrs:attr(GFP)), list(node_(GFP))) -> node_(GFP).
footer(Attrs, Children) ->
    {element, <<"footer"/utf8>>, Attrs, Children}.

-spec footer_text(list(nakai@html@attrs:attr(GFV)), binary()) -> node_(GFV).
footer_text(Attrs, Text) ->
    {element, <<"footer"/utf8>>, Attrs, [{text, Text}]}.

-spec form(list(nakai@html@attrs:attr(GFZ)), list(node_(GFZ))) -> node_(GFZ).
form(Attrs, Children) ->
    {element, <<"form"/utf8>>, Attrs, Children}.

-spec form_text(list(nakai@html@attrs:attr(GGF)), binary()) -> node_(GGF).
form_text(Attrs, Text) ->
    {element, <<"form"/utf8>>, Attrs, [{text, Text}]}.

-spec h1(list(nakai@html@attrs:attr(GGJ)), list(node_(GGJ))) -> node_(GGJ).
h1(Attrs, Children) ->
    {element, <<"h1"/utf8>>, Attrs, Children}.

-spec h1_text(list(nakai@html@attrs:attr(GGP)), binary()) -> node_(GGP).
h1_text(Attrs, Text) ->
    {element, <<"h1"/utf8>>, Attrs, [{text, Text}]}.

-spec h2(list(nakai@html@attrs:attr(GGT)), list(node_(GGT))) -> node_(GGT).
h2(Attrs, Children) ->
    {element, <<"h2"/utf8>>, Attrs, Children}.

-spec h2_text(list(nakai@html@attrs:attr(GGZ)), binary()) -> node_(GGZ).
h2_text(Attrs, Text) ->
    {element, <<"h2"/utf8>>, Attrs, [{text, Text}]}.

-spec h3(list(nakai@html@attrs:attr(GHD)), list(node_(GHD))) -> node_(GHD).
h3(Attrs, Children) ->
    {element, <<"h3"/utf8>>, Attrs, Children}.

-spec h3_text(list(nakai@html@attrs:attr(GHJ)), binary()) -> node_(GHJ).
h3_text(Attrs, Text) ->
    {element, <<"h3"/utf8>>, Attrs, [{text, Text}]}.

-spec h4(list(nakai@html@attrs:attr(GHN)), list(node_(GHN))) -> node_(GHN).
h4(Attrs, Children) ->
    {element, <<"h4"/utf8>>, Attrs, Children}.

-spec h4_text(list(nakai@html@attrs:attr(GHT)), binary()) -> node_(GHT).
h4_text(Attrs, Text) ->
    {element, <<"h4"/utf8>>, Attrs, [{text, Text}]}.

-spec h5(list(nakai@html@attrs:attr(GHX)), list(node_(GHX))) -> node_(GHX).
h5(Attrs, Children) ->
    {element, <<"h5"/utf8>>, Attrs, Children}.

-spec h5_text(list(nakai@html@attrs:attr(GID)), binary()) -> node_(GID).
h5_text(Attrs, Text) ->
    {element, <<"h5"/utf8>>, Attrs, [{text, Text}]}.

-spec h6(list(nakai@html@attrs:attr(GIH)), list(node_(GIH))) -> node_(GIH).
h6(Attrs, Children) ->
    {element, <<"h6"/utf8>>, Attrs, Children}.

-spec h6_text(list(nakai@html@attrs:attr(GIN)), binary()) -> node_(GIN).
h6_text(Attrs, Text) ->
    {element, <<"h6"/utf8>>, Attrs, [{text, Text}]}.

-spec header(list(nakai@html@attrs:attr(GIR)), list(node_(GIR))) -> node_(GIR).
header(Attrs, Children) ->
    {element, <<"header"/utf8>>, Attrs, Children}.

-spec header_text(list(nakai@html@attrs:attr(GIX)), binary()) -> node_(GIX).
header_text(Attrs, Text) ->
    {element, <<"header"/utf8>>, Attrs, [{text, Text}]}.

-spec hr(list(nakai@html@attrs:attr(GJB))) -> node_(GJB).
hr(Attrs) ->
    {leaf_element, <<"hr"/utf8>>, Attrs}.

-spec i(list(nakai@html@attrs:attr(GJF)), list(node_(GJF))) -> node_(GJF).
i(Attrs, Children) ->
    {element, <<"i"/utf8>>, Attrs, Children}.

-spec i_text(list(nakai@html@attrs:attr(GJL)), binary()) -> node_(GJL).
i_text(Attrs, Text) ->
    {element, <<"i"/utf8>>, Attrs, [{text, Text}]}.

-spec iframe(list(nakai@html@attrs:attr(GJP)), list(node_(GJP))) -> node_(GJP).
iframe(Attrs, Children) ->
    {element, <<"iframe"/utf8>>, Attrs, Children}.

-spec iframe_text(list(nakai@html@attrs:attr(GJV)), binary()) -> node_(GJV).
iframe_text(Attrs, Text) ->
    {element, <<"iframe"/utf8>>, Attrs, [{text, Text}]}.

-spec img(list(nakai@html@attrs:attr(GJZ))) -> node_(GJZ).
img(Attrs) ->
    {leaf_element, <<"img"/utf8>>, Attrs}.

-spec input(list(nakai@html@attrs:attr(GKD))) -> node_(GKD).
input(Attrs) ->
    {leaf_element, <<"input"/utf8>>, Attrs}.

-spec ins(list(nakai@html@attrs:attr(GKH)), list(node_(GKH))) -> node_(GKH).
ins(Attrs, Children) ->
    {element, <<"ins"/utf8>>, Attrs, Children}.

-spec ins_text(list(nakai@html@attrs:attr(GKN)), binary()) -> node_(GKN).
ins_text(Attrs, Text) ->
    {element, <<"ins"/utf8>>, Attrs, [{text, Text}]}.

-spec kbd(list(nakai@html@attrs:attr(GKR)), list(node_(GKR))) -> node_(GKR).
kbd(Attrs, Children) ->
    {element, <<"kbd"/utf8>>, Attrs, Children}.

-spec kbd_text(list(nakai@html@attrs:attr(GKX)), binary()) -> node_(GKX).
kbd_text(Attrs, Text) ->
    {element, <<"kbd"/utf8>>, Attrs, [{text, Text}]}.

-spec label(list(nakai@html@attrs:attr(GLB)), list(node_(GLB))) -> node_(GLB).
label(Attrs, Children) ->
    {element, <<"label"/utf8>>, Attrs, Children}.

-spec label_text(list(nakai@html@attrs:attr(GLH)), binary()) -> node_(GLH).
label_text(Attrs, Text) ->
    {element, <<"label"/utf8>>, Attrs, [{text, Text}]}.

-spec legend(list(nakai@html@attrs:attr(GLL)), list(node_(GLL))) -> node_(GLL).
legend(Attrs, Children) ->
    {element, <<"legend"/utf8>>, Attrs, Children}.

-spec legend_text(list(nakai@html@attrs:attr(GLR)), binary()) -> node_(GLR).
legend_text(Attrs, Text) ->
    {element, <<"legend"/utf8>>, Attrs, [{text, Text}]}.

-spec li(list(nakai@html@attrs:attr(GLV)), list(node_(GLV))) -> node_(GLV).
li(Attrs, Children) ->
    {element, <<"li"/utf8>>, Attrs, Children}.

-spec li_text(list(nakai@html@attrs:attr(GMB)), binary()) -> node_(GMB).
li_text(Attrs, Text) ->
    {element, <<"li"/utf8>>, Attrs, [{text, Text}]}.

-spec link(list(nakai@html@attrs:attr(GMF))) -> node_(GMF).
link(Attrs) ->
    {leaf_element, <<"link"/utf8>>, Attrs}.

-spec main(list(nakai@html@attrs:attr(GMJ)), list(node_(GMJ))) -> node_(GMJ).
main(Attrs, Children) ->
    {element, <<"main"/utf8>>, Attrs, Children}.

-spec main_text(list(nakai@html@attrs:attr(GMP)), binary()) -> node_(GMP).
main_text(Attrs, Text) ->
    {element, <<"main"/utf8>>, Attrs, [{text, Text}]}.

-spec map(list(nakai@html@attrs:attr(GMT)), list(node_(GMT))) -> node_(GMT).
map(Attrs, Children) ->
    {element, <<"map"/utf8>>, Attrs, Children}.

-spec map_text(list(nakai@html@attrs:attr(GMZ)), binary()) -> node_(GMZ).
map_text(Attrs, Text) ->
    {element, <<"map"/utf8>>, Attrs, [{text, Text}]}.

-spec mark(list(nakai@html@attrs:attr(GND)), list(node_(GND))) -> node_(GND).
mark(Attrs, Children) ->
    {element, <<"mark"/utf8>>, Attrs, Children}.

-spec mark_text(list(nakai@html@attrs:attr(GNJ)), binary()) -> node_(GNJ).
mark_text(Attrs, Text) ->
    {element, <<"mark"/utf8>>, Attrs, [{text, Text}]}.

-spec math(list(nakai@html@attrs:attr(GNN)), list(node_(GNN))) -> node_(GNN).
math(Attrs, Children) ->
    {element, <<"math"/utf8>>, Attrs, Children}.

-spec math_text(list(nakai@html@attrs:attr(GNT)), binary()) -> node_(GNT).
math_text(Attrs, Text) ->
    {element, <<"math"/utf8>>, Attrs, [{text, Text}]}.

-spec menu(list(nakai@html@attrs:attr(GNX)), list(node_(GNX))) -> node_(GNX).
menu(Attrs, Children) ->
    {element, <<"menu"/utf8>>, Attrs, Children}.

-spec menu_text(list(nakai@html@attrs:attr(GOD)), binary()) -> node_(GOD).
menu_text(Attrs, Text) ->
    {element, <<"menu"/utf8>>, Attrs, [{text, Text}]}.

-spec menuitem(list(nakai@html@attrs:attr(GOH)), list(node_(GOH))) -> node_(GOH).
menuitem(Attrs, Children) ->
    {element, <<"menuitem"/utf8>>, Attrs, Children}.

-spec menuitem_text(list(nakai@html@attrs:attr(GON)), binary()) -> node_(GON).
menuitem_text(Attrs, Text) ->
    {element, <<"menuitem"/utf8>>, Attrs, [{text, Text}]}.

-spec meta(list(nakai@html@attrs:attr(GOR))) -> node_(GOR).
meta(Attrs) ->
    {leaf_element, <<"meta"/utf8>>, Attrs}.

-spec meter(list(nakai@html@attrs:attr(GOV)), list(node_(GOV))) -> node_(GOV).
meter(Attrs, Children) ->
    {element, <<"meter"/utf8>>, Attrs, Children}.

-spec meter_text(list(nakai@html@attrs:attr(GPB)), binary()) -> node_(GPB).
meter_text(Attrs, Text) ->
    {element, <<"meter"/utf8>>, Attrs, [{text, Text}]}.

-spec nav(list(nakai@html@attrs:attr(GPF)), list(node_(GPF))) -> node_(GPF).
nav(Attrs, Children) ->
    {element, <<"nav"/utf8>>, Attrs, Children}.

-spec nav_text(list(nakai@html@attrs:attr(GPL)), binary()) -> node_(GPL).
nav_text(Attrs, Text) ->
    {element, <<"nav"/utf8>>, Attrs, [{text, Text}]}.

-spec noscript(list(nakai@html@attrs:attr(GPP)), list(node_(GPP))) -> node_(GPP).
noscript(Attrs, Children) ->
    {element, <<"noscript"/utf8>>, Attrs, Children}.

-spec noscript_text(list(nakai@html@attrs:attr(GPV)), binary()) -> node_(GPV).
noscript_text(Attrs, Text) ->
    {element, <<"noscript"/utf8>>, Attrs, [{text, Text}]}.

-spec object(list(nakai@html@attrs:attr(GPZ)), list(node_(GPZ))) -> node_(GPZ).
object(Attrs, Children) ->
    {element, <<"object"/utf8>>, Attrs, Children}.

-spec object_text(list(nakai@html@attrs:attr(GQF)), binary()) -> node_(GQF).
object_text(Attrs, Text) ->
    {element, <<"object"/utf8>>, Attrs, [{text, Text}]}.

-spec ol(list(nakai@html@attrs:attr(GQJ)), list(node_(GQJ))) -> node_(GQJ).
ol(Attrs, Children) ->
    {element, <<"ol"/utf8>>, Attrs, Children}.

-spec ol_text(list(nakai@html@attrs:attr(GQP)), binary()) -> node_(GQP).
ol_text(Attrs, Text) ->
    {element, <<"ol"/utf8>>, Attrs, [{text, Text}]}.

-spec optgroup(list(nakai@html@attrs:attr(GQT)), list(node_(GQT))) -> node_(GQT).
optgroup(Attrs, Children) ->
    {element, <<"optgroup"/utf8>>, Attrs, Children}.

-spec optgroup_text(list(nakai@html@attrs:attr(GQZ)), binary()) -> node_(GQZ).
optgroup_text(Attrs, Text) ->
    {element, <<"optgroup"/utf8>>, Attrs, [{text, Text}]}.

-spec option(list(nakai@html@attrs:attr(GRD)), list(node_(GRD))) -> node_(GRD).
option(Attrs, Children) ->
    {element, <<"option"/utf8>>, Attrs, Children}.

-spec option_text(list(nakai@html@attrs:attr(GRJ)), binary()) -> node_(GRJ).
option_text(Attrs, Text) ->
    {element, <<"option"/utf8>>, Attrs, [{text, Text}]}.

-spec output(list(nakai@html@attrs:attr(GRN)), list(node_(GRN))) -> node_(GRN).
output(Attrs, Children) ->
    {element, <<"output"/utf8>>, Attrs, Children}.

-spec output_text(list(nakai@html@attrs:attr(GRT)), binary()) -> node_(GRT).
output_text(Attrs, Text) ->
    {element, <<"output"/utf8>>, Attrs, [{text, Text}]}.

-spec p(list(nakai@html@attrs:attr(GRX)), list(node_(GRX))) -> node_(GRX).
p(Attrs, Children) ->
    {element, <<"p"/utf8>>, Attrs, Children}.

-spec p_text(list(nakai@html@attrs:attr(GSD)), binary()) -> node_(GSD).
p_text(Attrs, Text) ->
    {element, <<"p"/utf8>>, Attrs, [{text, Text}]}.

-spec param(list(nakai@html@attrs:attr(GSH)), list(node_(GSH))) -> node_(GSH).
param(Attrs, Children) ->
    {element, <<"param"/utf8>>, Attrs, Children}.

-spec param_text(list(nakai@html@attrs:attr(GSN)), binary()) -> node_(GSN).
param_text(Attrs, Text) ->
    {element, <<"param"/utf8>>, Attrs, [{text, Text}]}.

-spec picture(list(nakai@html@attrs:attr(GSR)), list(node_(GSR))) -> node_(GSR).
picture(Attrs, Children) ->
    {element, <<"picture"/utf8>>, Attrs, Children}.

-spec picture_text(list(nakai@html@attrs:attr(GSX)), binary()) -> node_(GSX).
picture_text(Attrs, Text) ->
    {element, <<"picture"/utf8>>, Attrs, [{text, Text}]}.

-spec pre(list(nakai@html@attrs:attr(GTB)), list(node_(GTB))) -> node_(GTB).
pre(Attrs, Children) ->
    {element, <<"pre"/utf8>>, Attrs, Children}.

-spec pre_text(list(nakai@html@attrs:attr(GTH)), binary()) -> node_(GTH).
pre_text(Attrs, Text) ->
    {element, <<"pre"/utf8>>, Attrs, [{text, Text}]}.

-spec progress(list(nakai@html@attrs:attr(GTL)), list(node_(GTL))) -> node_(GTL).
progress(Attrs, Children) ->
    {element, <<"progress"/utf8>>, Attrs, Children}.

-spec progress_text(list(nakai@html@attrs:attr(GTR)), binary()) -> node_(GTR).
progress_text(Attrs, Text) ->
    {element, <<"progress"/utf8>>, Attrs, [{text, Text}]}.

-spec q(list(nakai@html@attrs:attr(GTV)), list(node_(GTV))) -> node_(GTV).
q(Attrs, Children) ->
    {element, <<"q"/utf8>>, Attrs, Children}.

-spec q_text(list(nakai@html@attrs:attr(GUB)), binary()) -> node_(GUB).
q_text(Attrs, Text) ->
    {element, <<"q"/utf8>>, Attrs, [{text, Text}]}.

-spec rp(list(nakai@html@attrs:attr(GUF)), list(node_(GUF))) -> node_(GUF).
rp(Attrs, Children) ->
    {element, <<"rp"/utf8>>, Attrs, Children}.

-spec rp_text(list(nakai@html@attrs:attr(GUL)), binary()) -> node_(GUL).
rp_text(Attrs, Text) ->
    {element, <<"rp"/utf8>>, Attrs, [{text, Text}]}.

-spec rt(list(nakai@html@attrs:attr(GUP)), list(node_(GUP))) -> node_(GUP).
rt(Attrs, Children) ->
    {element, <<"rt"/utf8>>, Attrs, Children}.

-spec rt_text(list(nakai@html@attrs:attr(GUV)), binary()) -> node_(GUV).
rt_text(Attrs, Text) ->
    {element, <<"rt"/utf8>>, Attrs, [{text, Text}]}.

-spec ruby(list(nakai@html@attrs:attr(GUZ)), list(node_(GUZ))) -> node_(GUZ).
ruby(Attrs, Children) ->
    {element, <<"ruby"/utf8>>, Attrs, Children}.

-spec ruby_text(list(nakai@html@attrs:attr(GVF)), binary()) -> node_(GVF).
ruby_text(Attrs, Text) ->
    {element, <<"ruby"/utf8>>, Attrs, [{text, Text}]}.

-spec s(list(nakai@html@attrs:attr(GVJ)), list(node_(GVJ))) -> node_(GVJ).
s(Attrs, Children) ->
    {element, <<"s"/utf8>>, Attrs, Children}.

-spec s_text(list(nakai@html@attrs:attr(GVP)), binary()) -> node_(GVP).
s_text(Attrs, Text) ->
    {element, <<"s"/utf8>>, Attrs, [{text, Text}]}.

-spec samp(list(nakai@html@attrs:attr(GVT)), list(node_(GVT))) -> node_(GVT).
samp(Attrs, Children) ->
    {element, <<"samp"/utf8>>, Attrs, Children}.

-spec samp_text(list(nakai@html@attrs:attr(GVZ)), binary()) -> node_(GVZ).
samp_text(Attrs, Text) ->
    {element, <<"samp"/utf8>>, Attrs, [{text, Text}]}.

-spec section(list(nakai@html@attrs:attr(GWD)), list(node_(GWD))) -> node_(GWD).
section(Attrs, Children) ->
    {element, <<"section"/utf8>>, Attrs, Children}.

-spec section_text(list(nakai@html@attrs:attr(GWJ)), binary()) -> node_(GWJ).
section_text(Attrs, Text) ->
    {element, <<"section"/utf8>>, Attrs, [{text, Text}]}.

-spec select(list(nakai@html@attrs:attr(GWN)), list(node_(GWN))) -> node_(GWN).
select(Attrs, Children) ->
    {element, <<"select"/utf8>>, Attrs, Children}.

-spec select_text(list(nakai@html@attrs:attr(GWT)), binary()) -> node_(GWT).
select_text(Attrs, Text) ->
    {element, <<"select"/utf8>>, Attrs, [{text, Text}]}.

-spec small(list(nakai@html@attrs:attr(GWX)), list(node_(GWX))) -> node_(GWX).
small(Attrs, Children) ->
    {element, <<"small"/utf8>>, Attrs, Children}.

-spec small_text(list(nakai@html@attrs:attr(GXD)), binary()) -> node_(GXD).
small_text(Attrs, Text) ->
    {element, <<"small"/utf8>>, Attrs, [{text, Text}]}.

-spec source(list(nakai@html@attrs:attr(GXH))) -> node_(GXH).
source(Attrs) ->
    {leaf_element, <<"source"/utf8>>, Attrs}.

-spec span(list(nakai@html@attrs:attr(GXL)), list(node_(GXL))) -> node_(GXL).
span(Attrs, Children) ->
    {element, <<"span"/utf8>>, Attrs, Children}.

-spec span_text(list(nakai@html@attrs:attr(GXR)), binary()) -> node_(GXR).
span_text(Attrs, Text) ->
    {element, <<"span"/utf8>>, Attrs, [{text, Text}]}.

-spec strong(list(nakai@html@attrs:attr(GXV)), list(node_(GXV))) -> node_(GXV).
strong(Attrs, Children) ->
    {element, <<"strong"/utf8>>, Attrs, Children}.

-spec strong_text(list(nakai@html@attrs:attr(GYB)), binary()) -> node_(GYB).
strong_text(Attrs, Text) ->
    {element, <<"strong"/utf8>>, Attrs, [{text, Text}]}.

-spec sub(list(nakai@html@attrs:attr(GYF)), list(node_(GYF))) -> node_(GYF).
sub(Attrs, Children) ->
    {element, <<"sub"/utf8>>, Attrs, Children}.

-spec sub_text(list(nakai@html@attrs:attr(GYL)), binary()) -> node_(GYL).
sub_text(Attrs, Text) ->
    {element, <<"sub"/utf8>>, Attrs, [{text, Text}]}.

-spec summary(list(nakai@html@attrs:attr(GYP)), list(node_(GYP))) -> node_(GYP).
summary(Attrs, Children) ->
    {element, <<"summary"/utf8>>, Attrs, Children}.

-spec summary_text(list(nakai@html@attrs:attr(GYV)), binary()) -> node_(GYV).
summary_text(Attrs, Text) ->
    {element, <<"summary"/utf8>>, Attrs, [{text, Text}]}.

-spec sup(list(nakai@html@attrs:attr(GYZ)), list(node_(GYZ))) -> node_(GYZ).
sup(Attrs, Children) ->
    {element, <<"sup"/utf8>>, Attrs, Children}.

-spec sup_text(list(nakai@html@attrs:attr(GZF)), binary()) -> node_(GZF).
sup_text(Attrs, Text) ->
    {element, <<"sup"/utf8>>, Attrs, [{text, Text}]}.

-spec svg(list(nakai@html@attrs:attr(GZJ)), list(node_(GZJ))) -> node_(GZJ).
svg(Attrs, Children) ->
    {element, <<"svg"/utf8>>, Attrs, Children}.

-spec svg_text(list(nakai@html@attrs:attr(GZP)), binary()) -> node_(GZP).
svg_text(Attrs, Text) ->
    {element, <<"svg"/utf8>>, Attrs, [{text, Text}]}.

-spec table(list(nakai@html@attrs:attr(GZT)), list(node_(GZT))) -> node_(GZT).
table(Attrs, Children) ->
    {element, <<"table"/utf8>>, Attrs, Children}.

-spec table_text(list(nakai@html@attrs:attr(GZZ)), binary()) -> node_(GZZ).
table_text(Attrs, Text) ->
    {element, <<"table"/utf8>>, Attrs, [{text, Text}]}.

-spec tbody(list(nakai@html@attrs:attr(HAD)), list(node_(HAD))) -> node_(HAD).
tbody(Attrs, Children) ->
    {element, <<"tbody"/utf8>>, Attrs, Children}.

-spec tbody_text(list(nakai@html@attrs:attr(HAJ)), binary()) -> node_(HAJ).
tbody_text(Attrs, Text) ->
    {element, <<"tbody"/utf8>>, Attrs, [{text, Text}]}.

-spec td(list(nakai@html@attrs:attr(HAN)), list(node_(HAN))) -> node_(HAN).
td(Attrs, Children) ->
    {element, <<"td"/utf8>>, Attrs, Children}.

-spec td_text(list(nakai@html@attrs:attr(HAT)), binary()) -> node_(HAT).
td_text(Attrs, Text) ->
    {element, <<"td"/utf8>>, Attrs, [{text, Text}]}.

-spec textarea(list(nakai@html@attrs:attr(HAX)), list(node_(HAX))) -> node_(HAX).
textarea(Attrs, Children) ->
    {element, <<"textarea"/utf8>>, Attrs, Children}.

-spec textarea_text(list(nakai@html@attrs:attr(HBD)), binary()) -> node_(HBD).
textarea_text(Attrs, Text) ->
    {element, <<"textarea"/utf8>>, Attrs, [{text, Text}]}.

-spec tfoot(list(nakai@html@attrs:attr(HBH)), list(node_(HBH))) -> node_(HBH).
tfoot(Attrs, Children) ->
    {element, <<"tfoot"/utf8>>, Attrs, Children}.

-spec tfoot_text(list(nakai@html@attrs:attr(HBN)), binary()) -> node_(HBN).
tfoot_text(Attrs, Text) ->
    {element, <<"tfoot"/utf8>>, Attrs, [{text, Text}]}.

-spec th(list(nakai@html@attrs:attr(HBR)), list(node_(HBR))) -> node_(HBR).
th(Attrs, Children) ->
    {element, <<"th"/utf8>>, Attrs, Children}.

-spec th_text(list(nakai@html@attrs:attr(HBX)), binary()) -> node_(HBX).
th_text(Attrs, Text) ->
    {element, <<"th"/utf8>>, Attrs, [{text, Text}]}.

-spec thead(list(nakai@html@attrs:attr(HCB)), list(node_(HCB))) -> node_(HCB).
thead(Attrs, Children) ->
    {element, <<"thead"/utf8>>, Attrs, Children}.

-spec thead_text(list(nakai@html@attrs:attr(HCH)), binary()) -> node_(HCH).
thead_text(Attrs, Text) ->
    {element, <<"thead"/utf8>>, Attrs, [{text, Text}]}.

-spec time(list(nakai@html@attrs:attr(HCL)), list(node_(HCL))) -> node_(HCL).
time(Attrs, Children) ->
    {element, <<"time"/utf8>>, Attrs, Children}.

-spec time_text(list(nakai@html@attrs:attr(HCR)), binary()) -> node_(HCR).
time_text(Attrs, Text) ->
    {element, <<"time"/utf8>>, Attrs, [{text, Text}]}.

-spec tr(list(nakai@html@attrs:attr(HCV)), list(node_(HCV))) -> node_(HCV).
tr(Attrs, Children) ->
    {element, <<"tr"/utf8>>, Attrs, Children}.

-spec tr_text(list(nakai@html@attrs:attr(HDB)), binary()) -> node_(HDB).
tr_text(Attrs, Text) ->
    {element, <<"tr"/utf8>>, Attrs, [{text, Text}]}.

-spec track(list(nakai@html@attrs:attr(HDF))) -> node_(HDF).
track(Attrs) ->
    {leaf_element, <<"track"/utf8>>, Attrs}.

-spec u(list(nakai@html@attrs:attr(HDJ)), list(node_(HDJ))) -> node_(HDJ).
u(Attrs, Children) ->
    {element, <<"u"/utf8>>, Attrs, Children}.

-spec u_text(list(nakai@html@attrs:attr(HDP)), binary()) -> node_(HDP).
u_text(Attrs, Text) ->
    {element, <<"u"/utf8>>, Attrs, [{text, Text}]}.

-spec ul(list(nakai@html@attrs:attr(HDT)), list(node_(HDT))) -> node_(HDT).
ul(Attrs, Children) ->
    {element, <<"ul"/utf8>>, Attrs, Children}.

-spec ul_text(list(nakai@html@attrs:attr(HDZ)), binary()) -> node_(HDZ).
ul_text(Attrs, Text) ->
    {element, <<"ul"/utf8>>, Attrs, [{text, Text}]}.

-spec var(list(nakai@html@attrs:attr(HED)), list(node_(HED))) -> node_(HED).
var(Attrs, Children) ->
    {element, <<"var"/utf8>>, Attrs, Children}.

-spec var_text(list(nakai@html@attrs:attr(HEJ)), binary()) -> node_(HEJ).
var_text(Attrs, Text) ->
    {element, <<"var"/utf8>>, Attrs, [{text, Text}]}.

-spec video(list(nakai@html@attrs:attr(HEN)), list(node_(HEN))) -> node_(HEN).
video(Attrs, Children) ->
    {element, <<"video"/utf8>>, Attrs, Children}.

-spec video_text(list(nakai@html@attrs:attr(HET)), binary()) -> node_(HET).
video_text(Attrs, Text) ->
    {element, <<"video"/utf8>>, Attrs, [{text, Text}]}.

-spec wbr(list(nakai@html@attrs:attr(HEX)), list(node_(HEX))) -> node_(HEX).
wbr(Attrs, Children) ->
    {element, <<"wbr"/utf8>>, Attrs, Children}.

-spec wbr_text(list(nakai@html@attrs:attr(HFD)), binary()) -> node_(HFD).
wbr_text(Attrs, Text) ->
    {element, <<"wbr"/utf8>>, Attrs, [{text, Text}]}.
