;; MusicXML to Lilypond to Denemo
;; A Lexer / Parser to import ly-converted mxml files into Denemo
;; By Richard Shann and Nils Gey, July / August 2010
;; Usage of SILex and LALR-scm 
;;;;;;;;;;;;;;;;;;;;;;;;

;(hashq-set! nwc:Nwc2LyTable '0 "b'")


;;TOC
;; Libs
;; init input port
;; load and init lexer
;; lalr-parser definition
;; execute the parser
;; cleaning up (input port...)
;;;;;

;; Libs
(load "lalr.scm")
(load "silex.scm")

;; Input Port
(set-current-input-port (open-input-file "test.ly"))


;; Lexer
(define (mtoken symbol value) 
	(make-lexical-token symbol (make-source-location (current-input-port) (lexer-get-line) (lexer-get-column) (lexer-get-offset) -1) value)
)

(lex "lilypondlexer.l" "/home/nils/git-denemo/actions/mxml2ly2denemo/nwctext.l.scm" 'counters 'all) ; Oh no!! The generated scm file has comments in the language of the devil!
(load "lilypondlexer.l.scm")
(lexer-init 'port (current-input-port)) 




;; Parser Definition

;Helper to print out a value with a custom description, for console output

(define (display-combo string value)
	(display string)
	(display ": ")
	(display value)
	(newline)
) 

(define lilypond-parser

  (lalr-parser
   ;; --- token definitions
   ( ACCEPTS
;;;many more omitted here!!!
     TONICNAME_PITCH
  )
	
;;;;;;;;;;;; rules




lilypond:	/* empty */
	| lilypond toplevel_expression {
	}
;; 	| lilypond assignment {
;; 	}
;; 	| lilypond error {
;; 		PARSER->error_level_ = 1;
;; 	}
;; 	| lilypond INVALID	{
;; 		PARSER->error_level_ = 1;
;; 	}
;; 	;


toplevel_expression:
;; 	lilypond_header {
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$defaultheader"), $1);
;; 	}
;; 	| book_block {
;; 		Book *book = $1;
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-book-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), book->self_scm ());
;; 		book->unprotect ();
;; 	}
;; 	| bookpart_block {
;; 		Book *bookpart = $1;
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-bookpart-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), bookpart->self_scm ());
;; 		bookpart->unprotect ();
;; 	}
;; 	| score_block {
;; 		Score *score = $1;

;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-score-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), score->self_scm ());
;; 		score->unprotect ();
;; 	}


 	(composite_music ()) ;; {
;; 		Music *music = unsmob_music ($1);
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-music-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), music->self_scm ());
;; 	}
;; 	| full_markup {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-text-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), scm_list_1 ($1));
;; 	}
;; 	| full_markup_list {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("toplevel-text-handler");
;; 		scm_call_2 (proc, PARSER->self_scm (), $1);
;; 	}
;; 	| output_def {
;; 		SCM id = SCM_EOL;
;; 		Output_def * od = $1;

;; 		if ($1->c_variable ("is-paper") == SCM_BOOL_T)
;; 			id = ly_symbol2scm ("$defaultpaper");
;; 		else if ($1->c_variable ("is-midi") == SCM_BOOL_T)
;; 			id = ly_symbol2scm ("$defaultmidi");
;; 		else if ($1->c_variable ("is-layout") == SCM_BOOL_T)
;; 			id = ly_symbol2scm ("$defaultlayout");

;; 		PARSER->lexer_->set_identifier (id, od->self_scm ());
;; 		od->unprotect();
;; 	}
;; 	;

;; embedded_scm:
;; 	SCM_TOKEN
;; 	| SCM_IDENTIFIER
;; 	;


;; lilypond_header_body:
;; 	{
;; 		$$ = get_header (PARSER);
;; 		PARSER->lexer_->add_scope ($$);
;; 	}
;; 	| lilypond_header_body assignment  {

;; 	}
;; 	;

;; lilypond_header:
;; 	HEADER '{' lilypond_header_body '}'	{
;; 		$$ = PARSER->lexer_->remove_scope ();
;; 	}
;; 	;

;; /*
;; 	DECLARATIONS
;; */
;; assignment_id:
;; 	STRING		{ $$ = $1; }
;; 	| LYRICS_STRING { $$ = $1; }
;; 	;

;; assignment:
;; 	assignment_id '=' identifier_init  {
;; 	        PARSER->lexer_->set_identifier ($1, $3);
;; 	}
;; 	| assignment_id property_path '=' identifier_init {
;; 		SCM path = scm_cons (scm_string_to_symbol ($1), $2);
;; 		PARSER->lexer_->set_identifier (path, $4);
;; 	;
;; /*
;;  TODO: devise standard for protection in parser.

;;   The parser stack lives on the C-stack, which means that
;; all objects can be unprotected as soon as they're here.

;; */
;; 	}
;; 	| embedded_scm { }
;; 	;


;; identifier_init:
;; 	score_block {
;; 		$$ = $1->self_scm ();
;; 		$1->unprotect ();
;; 	}
;; 	| book_block {
;; 		$$ = $1->self_scm ();
;; 		$1->unprotect ();
;; 	}
;; 	| bookpart_block {
;; 		$$ = $1->self_scm ();
;; 		$1->unprotect ();
;; 	}
;; 	| output_def {
;; 		$$ = $1->self_scm ();
;; 		$1->unprotect ();
;; 	}
;; 	| context_def_spec_block {
;; 		$$ = $1;
;; 	}
;; 	| music  {
;; 		/* Hack: Create event-chord around standalone events.
;; 		   Prevents the identifier from being interpreted as a post-event. */
;; 		Music *mus = unsmob_music ($1);
;; 		bool is_event = mus &&
;; 			(scm_memq (ly_symbol2scm ("event"), mus->get_property ("types"))
;; 				!= SCM_BOOL_F);
;; 		if (!is_event)
;; 			$$ = $1;
;; 		else
;; 			$$ = MAKE_SYNTAX ("event-chord", @$, scm_list_1 ($1));
;; 	}
;; 	| post_event {
;; 		$$ = $1;
;; 	}
;; 	| number_expression {
;;  		$$ = $1;
;; 	}
;; 	| string {
;; 		$$ = $1;
;; 	}
;;         | embedded_scm {
;; 		$$ = $1;
;; 	}
;; 	| full_markup {
;; 		$$ = $1;
;; 	}
;; 	| DIGIT {
;; 		$$ = scm_from_int ($1);
;; 	}
;;         | context_modification {
;;                 $$ = $1;
;;         }
;; 	;

;; context_def_spec_block:
;; 	CONTEXT '{' context_def_spec_body '}'
;; 		{
;; 		$$ = $3;
;; 	}
;; 	;

;; context_def_spec_body:
;; 	/**/ {
;; 		$$ = Context_def::make_scm ();
;; 		unsmob_context_def ($$)->origin ()->set_spot (@$);
;; 	}
;; 	| CONTEXT_DEF_IDENTIFIER {
;; 		$$ = $1;
;; 		unsmob_context_def ($$)->origin ()->set_spot (@$);
;; 	}
;; 	| context_def_spec_body GROBDESCRIPTIONS embedded_scm {
;; 		Context_def*td = unsmob_context_def ($$);

;; 		for (SCM p = $3; scm_is_pair (p); p = scm_cdr (p)) {
;; 			SCM tag = scm_caar (p);

;; 			/* TODO: should make new tag "grob-definition" ? */
;; 			td->add_context_mod (scm_list_3 (ly_symbol2scm ("assign"),
;; 							tag, scm_cons (scm_cdar (p), SCM_EOL)));
;; 		}
;; 	}
;; 	| context_def_spec_body context_mod {
;; 		unsmob_context_def ($$)->add_context_mod ($2);
;; 	}
;; 	| context_def_spec_body context_modification {
;;                 Context_def *td = unsmob_context_def ($$);
;;                 SCM new_mods = unsmob_context_mod ($2)->get_mods ();
;;                 for (SCM m = new_mods; scm_is_pair (m); m = scm_cdr (m)) {
;;                     td->add_context_mod (scm_car (m));
;;                 }
;; 	}
;; 	;



;; book_block:
;; 	BOOK '{' book_body '}' 	{
;; 		$$ = $3;
;; 		pop_paper (PARSER);
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-book"), SCM_BOOL_F);
;; 	}
;; 	;

;; /* FIXME:
;;    * Use 'handlers' like for toplevel-* stuff?
;;    * grok \layout and \midi?  */
;; book_body:
;; 	{
;; 		$$ = new Book;
;; 		init_papers (PARSER);
;; 		$$->origin ()->set_spot (@$);
;; 		$$->paper_ = dynamic_cast<Output_def*> (unsmob_output_def (PARSER->lexer_->lookup_identifier ("$defaultpaper"))->clone ());
;; 		$$->paper_->unprotect ();
;; 		push_paper (PARSER, $$->paper_);
;; 		$$->header_ = PARSER->lexer_->lookup_identifier ("$defaultheader");
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $$->self_scm ());
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("book-output-suffix"), SCM_BOOL_F);
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("book-filename"), SCM_BOOL_F);
;; 	}
;; 	| BOOK_IDENTIFIER {
;; 		$$ = unsmob_book ($1);
;; 		$$->protect ();
;; 		$$->origin ()->set_spot (@$);
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-book"), $1);
;; 	}
;; 	| book_body paper_block {
;; 		$$->paper_ = $2;
;; 		$2->unprotect ();
;; 		set_paper (PARSER, $2);
;; 	}
;; 	| book_body bookpart_block {
;; 		Book *bookpart = $2;
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("book-bookpart-handler");
;; 		scm_call_2 (proc, $$->self_scm (), bookpart->self_scm ());
;; 		bookpart->unprotect ();
;; 	}
;; 	| book_body score_block {
;; 		Score *score = $2;
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("book-score-handler");
;; 		scm_call_2 (proc, $$->self_scm (), score->self_scm ());
;; 		score->unprotect ();
;; 	}
;; 	| book_body composite_music {
;; 		Music *music = unsmob_music ($2);
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("book-music-handler");
;; 		scm_call_3 (proc, PARSER->self_scm (), $$->self_scm (), music->self_scm ());
;; 	}
;; 	| book_body full_markup {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("book-text-handler");
;; 		scm_call_2 (proc, $$->self_scm (), scm_list_1 ($2));
;; 	}
;; 	| book_body full_markup_list {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("book-text-handler");
;; 		scm_call_2 (proc, $$->self_scm (), $2);
;; 	}
;; 	| book_body lilypond_header {
;; 		$$->header_ = $2;
;; 	}
;; 	| book_body error {
;; 		$$->paper_ = 0;
;; 		$$->scores_ = SCM_EOL;
;; 		$$->bookparts_ = SCM_EOL;
;; 	}
;; 	;

;; bookpart_block:
;; 	BOOKPART '{' bookpart_body '}' {
;; 		$$ = $3;
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), SCM_BOOL_F);
;; 	}
;; 	;

;; bookpart_body:
;; 	{
;; 		$$ = new Book;
;; 		$$->origin ()->set_spot (@$);
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $$->self_scm ());
;; 	}
;; 	| BOOK_IDENTIFIER {
;; 		$$ = unsmob_book ($1);
;; 		$$->protect ();
;; 		$$->origin ()->set_spot (@$);
;; 		PARSER->lexer_->set_identifier (ly_symbol2scm ("$current-bookpart"), $1);
;; 	}
;; 	| bookpart_body paper_block {
;; 		$$->paper_ = $2;
;; 		$2->unprotect ();
;; 	}
;; 	| bookpart_body score_block {
;; 		Score *score = $2;
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("bookpart-score-handler");
;; 		scm_call_2 (proc, $$->self_scm (), score->self_scm ());
;; 		score->unprotect ();
;; 	}
;; 	| bookpart_body composite_music {
;; 		Music *music = unsmob_music ($2);
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("bookpart-music-handler");
;; 		scm_call_3 (proc, PARSER->self_scm (), $$->self_scm (), music->self_scm ());
;; 	}
;; 	| bookpart_body full_markup {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("bookpart-text-handler");
;; 		scm_call_2 (proc, $$->self_scm (), scm_list_1 ($2));
;; 	}
;; 	| bookpart_body full_markup_list {
;; 		SCM proc = PARSER->lexer_->lookup_identifier ("bookpart-text-handler");
;; 		scm_call_2 (proc, $$->self_scm (), $2);
;; 	}
;; 	| bookpart_body lilypond_header {
;; 		$$->header_ = $2;
;; 	}
;; 	| bookpart_body error {
;; 		$$->paper_ = 0;
;; 		$$->scores_ = SCM_EOL;
;; 	}
;; 	;

;; score_block:
;; 	SCORE '{' score_body '}' 	{
;; 		$$ = $3;
;; 	}
;; 	;

;; score_body:
;; 	music {
;; 		SCM m = $1;
;; 		SCM scorify = ly_lily_module_constant ("scorify-music");
;; 		SCM score = scm_call_2 (scorify, m, PARSER->self_scm ());

;; 		// pass ownernship to C++ again.
;; 		$$ = unsmob_score (score);
;; 		$$->protect ();
;; 		$$->origin ()->set_spot (@$);
;; 	}
;; 	| SCORE_IDENTIFIER {
;; 		$$ = unsmob_score ($1);
;; 		$$->protect ();
;; 		$$->origin ()->set_spot (@$);
;; 	}
;; 	| score_body lilypond_header 	{
;; 		$$->set_header ($2);
;; 	}
;; 	| score_body output_def {
;; 		if ($2->lookup_variable (ly_symbol2scm ("is-paper")) == SCM_BOOL_T)
;; 		{
;; 			PARSER->parser_error (@2, _("\\paper cannot be used in \\score, use \\layout instead"));

;; 		}
;; 		else
;; 		{
;; 			$$->add_output_def ($2);
;; 		}
;; 		$2->unprotect ();
;; 	}
;; 	| score_body error {
;; 		$$->error_found_ = true;
;; 	}
;; 	;


;; /*
;; 	OUTPUT DEF
;; */

;; paper_block:
;; 	output_def {
;; 		$$ = $1;
;; 		if ($$->lookup_variable (ly_symbol2scm ("is-paper")) != SCM_BOOL_T)
;; 		{
;; 			PARSER->parser_error (@1, _ ("need \\paper for paper block"));
;; 			$1->unprotect ();
;; 			$$ = get_paper (PARSER);
;; 		}
;; 	}
;; 	;


;; output_def:
;; 	output_def_body '}' {
;; 		$$ = $1;

;; 		PARSER->lexer_->remove_scope ();
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	;

;; output_def_head:
;; 	PAPER {
;; 		$$ = get_paper (PARSER);
;; 		$$->input_origin_ = @$;
;; 		PARSER->lexer_->add_scope ($$->scope_);
;; 	}
;; 	| MIDI    {
;; 		Output_def *p = get_midi (PARSER);
;; 		$$ = p;
;; 		PARSER->lexer_->add_scope (p->scope_);
;; 	}
;; 	| LAYOUT 	{
;; 		Output_def *p = get_layout (PARSER);

;; 		PARSER->lexer_->add_scope (p->scope_);
;; 		$$ = p;
;; 	}
;; 	;

;; output_def_head_with_mode_switch:
;; 	output_def_head {
;; 		PARSER->lexer_->push_initial_state ();
;; 		$$ = $1;
;; 	}
;; 	;

;; output_def_body:
;; 	output_def_head_with_mode_switch '{' {
;; 		$$ = $1;
;; 		$$->input_origin_.set_spot (@$);
;; 	}
;; 	| output_def_head_with_mode_switch '{' OUTPUT_DEF_IDENTIFIER 	{
;; 		$1->unprotect ();

;; 		Output_def *o = unsmob_output_def ($3);
;; 		o->input_origin_.set_spot (@$);
;; 		$$ = o;
;; 		$$->protect ();
;; 		PARSER->lexer_->remove_scope ();
;; 		PARSER->lexer_->add_scope (o->scope_);
;; 	}
;; 	| output_def_body assignment  {

;; 	}
;; 	| output_def_body context_def_spec_block	{
;; 		assign_context_def ($$, $2);
;; 	}
;; 	| output_def_body error {

;; 	}
;; 	;

;; tempo_event:
;; 	TEMPO steno_duration '=' bare_unsigned	{
;; 		$$ = MAKE_SYNTAX ("tempo", @$, SCM_BOOL_F, $2, scm_int2num ($4));
;; 	}
;; 	| TEMPO string steno_duration '=' bare_unsigned	{
;; 		$$ = MAKE_SYNTAX ("tempo", @$, make_simple_markup($2), $3, scm_int2num ($5));
;; 	}
;; 	| TEMPO full_markup steno_duration '=' bare_unsigned	{
;; 		$$ = MAKE_SYNTAX ("tempo", @$, $2, $3, scm_int2num ($5));
;; 	}
;; 	| TEMPO string {
;; 		$$ = MAKE_SYNTAX ("tempoText", @$, make_simple_markup($2) );
;; 	}
;; 	| TEMPO full_markup {
;; 		$$ = MAKE_SYNTAX ("tempoText", @$, $2 );
;; 	}
;; 	;

;; /*
;; The representation of a  list is the

;;   (LIST . LAST-CONS)

;;  to have efficient append.  */

(music_list
;; 	/* empty */ {
;; 		$$ = scm_cons (SCM_EOL, SCM_EOL);
;; 	}
 (music_list music) : () ;; {
;; 		SCM s = $$;
;;  		SCM c = scm_cons ($2, SCM_EOL);

;; 		if (scm_is_pair (scm_cdr (s)))
;; 			scm_set_cdr_x (scm_cdr (s), c); /* append */
;; 		else
;; 			scm_set_car_x (s, c); /* set first cons */
;; 		scm_set_cdr_x (s, c);  /* remember last cell */
;; 	}
;; 	| music_list embedded_scm {

;; 	}
;; 	| music_list error {
;; 		Music *m = MY_MAKE_MUSIC("Music", @$);
;; 		// ugh. code dup
;; 		m->set_property ("error-found", SCM_BOOL_T);
;; 		SCM s = $$;
;;  		SCM c = scm_cons (m->self_scm (), SCM_EOL);
;; 		m->unprotect (); /* UGH */

;; 		if (scm_is_pair (scm_cdr (s)))
;; 			scm_set_cdr_x (scm_cdr (s), c); /* append */
;; 		else
;; 			scm_set_car_x (s, c); /* set first cons */
;; 		scm_set_cdr_x (s, c);  /* remember last cell */
;; 	}
;; 	;
)
(music
;; 	simple_music
 (composite_music) :  ()
;; 	;
)
;; alternative_music:
;; 	/* empty */ {
;; 		$$ = SCM_EOL;
;; 	}
;; 	| ALTERNATIVE '{' music_list '}' {
;; 		$$ = scm_car ($3);
;; 	}
;; 	;


;; repeated_music:
;; 	REPEAT simple_string unsigned_number music alternative_music
;; 	{
;; 		$$ = MAKE_SYNTAX ("repeat", @$, $2, $3, $4, $5);
;; 	}
;; 	;

(sequential_music
;; 	SEQUENTIAL '{' music_list '}'		{
;; 		$$ = MAKE_SYNTAX ("sequential-music", @$, scm_car ($3));
;; 	}
 ('{' music_list '}' : ()) ;;		{
;; 		$$ = MAKE_SYNTAX ("sequential-music", @$, scm_car ($2));
;; 	}
;; 	;
)

;; simultaneous_music:
;; 	SIMULTANEOUS '{' music_list '}'{
;; 		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($3));
;; 	}
;; 	| DOUBLE_ANGLE_OPEN music_list DOUBLE_ANGLE_CLOSE	{
;; 		$$ = MAKE_SYNTAX ("simultaneous-music", @$, scm_car ($2));
;; 	}
;; 	;

;; simple_music:
;; 	event_chord
;; 	| MUSIC_IDENTIFIER
;; 	| music_property_def
;; 	| context_change
;; 	;

;; context_modification:
;;         WITH { PARSER->lexer_->push_initial_state (); } '{' context_mod_list '}'
;;         {
;;                 PARSER->lexer_->pop_state ();
;;                 $$ = $4;
;;         }
;;         | WITH CONTEXT_MOD_IDENTIFIER
;;         {
;;                 $$ = $2;
;;         }
;;         | CONTEXT_MOD_IDENTIFIER
;;         {
;;                 $$ = $1;
;;         }
;;         ;

;; optional_context_mod:
;;         /**/ {
;;             $$ = SCM_EOL;
;;         }
;;         | context_modification
;;         {
;;               $$ = $1;
;;         }
;;         ;

;; context_mod_list:
;;         /**/ {
;;             $$ = Context_mod ().smobbed_copy ();
;;         }
;;         | context_mod_list context_mod  {
;;                  unsmob_context_mod ($1)->add_context_mod ($2);
;;         }
;;         | context_mod_list CONTEXT_MOD_IDENTIFIER {
;;                  Context_mod *md = unsmob_context_mod ($2);
;;                  if (md)
;;                      unsmob_context_mod ($1)->add_context_mods (md->get_mods ());
;;         }
;;         ;

(composite_music
;; 	prefix_composite_music { $$ = $1; }
 (grouped_music_list) : () ;; { $$ = $1; }
;; 	;
)
(grouped_music_list
 ;;;(simultaneous_music) : () ;;; did not mean to do this one yet{ $$ = $1; }
(sequential_music) : () ;;;		{ $$ = $1; }
;; 	;
)
;; function_scm_argument:
;; 	embedded_scm
;; 	| simple_string
;; 	;

;; /* An argument list. If a function \foo expects scm scm music, then the lexer expands \foo into the token sequence:
;;  MUSIC_FUNCTION EXPECT_MUSIC EXPECT_SCM EXPECT_SCM EXPECT_NO_MORE_ARGS
;; and this rule returns the reversed list of arguments. */

;; function_arglist_music_last:
;; 	EXPECT_MUSIC function_arglist music {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	;

;; function_arglist_nonmusic_last:
;; 	EXPECT_MARKUP function_arglist full_markup {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_MARKUP function_arglist simple_string {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_SCM function_arglist function_scm_argument {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	;

;; function_arglist_nonmusic: EXPECT_NO_MORE_ARGS {
;; 		$$ = SCM_EOL;
;; 	}
;; 	| EXPECT_MARKUP function_arglist_nonmusic full_markup {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_MARKUP function_arglist_nonmusic simple_string {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_SCM function_arglist_nonmusic function_scm_argument {
;; 		$$ = scm_cons ($3, $2);
;; 	}
;; 	;

;; function_arglist: EXPECT_NO_MORE_ARGS {
;; 		/* This is for 0-ary functions, so they don't need to
;; 		   read a lookahead token */
;; 		$$ = SCM_EOL;
;; 	}
;; 	| function_arglist_music_last
;; 	| function_arglist_nonmusic_last
;; 	;

;; generic_prefix_music_scm:
;; 	MUSIC_FUNCTION function_arglist {
;; 		$$ = ly_append2 (scm_list_2 ($1, make_input (@$)), scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	;


;; optional_id:
;; 	/**/ { $$ = SCM_EOL; }
;; 	| '=' simple_string {
;; 		$$ = $2;
;; 	}
;; 	;


;; prefix_composite_music:
;; 	generic_prefix_music_scm {
;; 		$$ = run_music_function (PARSER, $1);
;; 	}
;; 	| CONTEXT simple_string optional_id optional_context_mod music {
;;                 Context_mod *ctxmod = unsmob_context_mod ($4);
;;                 SCM mods = SCM_EOL;
;;                 if (ctxmod)
;;                         mods = ctxmod->get_mods ();
;; 		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_F);
;; 	}
;; 	| NEWCONTEXT simple_string optional_id optional_context_mod music {
;;                 Context_mod *ctxmod = unsmob_context_mod ($4);
;;                 SCM mods = SCM_EOL;
;;                 if (ctxmod)
;;                         mods = ctxmod->get_mods ();
;; 		$$ = MAKE_SYNTAX ("context-specification", @$, $2, $3, $5, mods, SCM_BOOL_T);
;; 	}

;; 	| TIMES fraction music {
;;                 $$ = MAKE_SYNTAX ("time-scaled-music", @$, $2, $3);
;; 	}
;; 	| repeated_music		{ $$ = $1; }
;; 	| TRANSPOSE pitch_also_in_chords pitch_also_in_chords music {
;; 		Pitch from = *unsmob_pitch ($2);
;; 		Pitch to = *unsmob_pitch ($3);
;; 		SCM pitch = pitch_interval (from, to).smobbed_copy ();
;; 		$$ = MAKE_SYNTAX ("transpose-music", @$, pitch, $4);
;; 	}
;; 	| mode_changing_head grouped_music_list {
;; 		if ($1 == ly_symbol2scm ("chords"))
;; 		{
;; 		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $2);
;; 		}
;; 		else
;; 		{
;; 		  $$ = $2;
;; 		}
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	| mode_changing_head_with_context optional_context_mod grouped_music_list {
;;                 Context_mod *ctxmod = unsmob_context_mod ($2);
;;                 SCM mods = SCM_EOL;
;;                 if (ctxmod)
;;                         mods = ctxmod->get_mods ();
;; 		$$ = MAKE_SYNTAX ("context-specification", @$, $1, SCM_EOL, $3, mods, SCM_BOOL_T);
;; 		if ($1 == ly_symbol2scm ("ChordNames"))
;; 		{
;; 		  $$ = MAKE_SYNTAX ("unrelativable-music", @$, $$);
;; 		}
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	| relative_music	{ $$ = $1; }
;; 	| re_rhythmed_music	{ $$ = $1; }
;; 	;

;; mode_changing_head:
;; 	NOTEMODE {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("pitchnames");
;; 		PARSER->lexer_->push_note_state (alist_to_hashq (nn));

;; 		$$ = ly_symbol2scm ("notes");
;; 	}
;; 	| DRUMMODE
;; 		{
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("drumPitchNames");
;; 		PARSER->lexer_->push_note_state (alist_to_hashq (nn));

;; 		$$ = ly_symbol2scm ("drums");
;; 	}
;; 	| FIGUREMODE {
;; 		PARSER->lexer_->push_figuredbass_state ();

;; 		$$ = ly_symbol2scm ("figures");
;; 	}
;; 	| CHORDMODE {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("chordmodifiers");
;; 		PARSER->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
;; 		nn = PARSER->lexer_->lookup_identifier ("pitchnames");
;; 		PARSER->lexer_->push_chord_state (alist_to_hashq (nn));
;; 		$$ = ly_symbol2scm ("chords");

;; 	}
;; 	| LYRICMODE
;; 		{ PARSER->lexer_->push_lyric_state ();
;; 		$$ = ly_symbol2scm ("lyrics");
;; 	}
;; 	;

;; mode_changing_head_with_context:
;; 	DRUMS {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("drumPitchNames");
;; 		PARSER->lexer_->push_note_state (alist_to_hashq (nn));

;; 		$$ = ly_symbol2scm ("DrumStaff");
;; 	}
;; 	| FIGURES {
;; 		PARSER->lexer_->push_figuredbass_state ();

;; 		$$ = ly_symbol2scm ("FiguredBass");
;; 	}
;; 	| CHORDS {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("chordmodifiers");
;; 		PARSER->lexer_->chordmodifier_tab_ = alist_to_hashq (nn);
;; 		nn = PARSER->lexer_->lookup_identifier ("pitchnames");
;; 		PARSER->lexer_->push_chord_state (alist_to_hashq (nn));
;; 		$$ = ly_symbol2scm ("ChordNames");
;; 	}
;; 	| LYRICS
;; 		{ PARSER->lexer_->push_lyric_state ();
;; 		$$ = ly_symbol2scm ("Lyrics");
;; 	}
;; 	;


;; relative_music:
;; 	RELATIVE absolute_pitch music {
;; 		Pitch start = *unsmob_pitch ($2);
;; 		$$ = make_music_relative (start, $3, @$);
;; 	}
;; 	| RELATIVE composite_music {
;; 		Pitch middle_c (0, 0, 0);
;; 		$$ = make_music_relative (middle_c, $2, @$);
;; 	}
;; 	;

;; new_lyrics:
;; 	ADDLYRICS { PARSER->lexer_->push_lyric_state (); }
;; 	/*cont */
;; 	grouped_music_list {
;; 	/* Can also use music at the expensive of two S/Rs similar to
;;            \repeat \alternative */
;; 		PARSER->lexer_->pop_state ();

;; 		$$ = scm_cons ($3, SCM_EOL);
;; 	}
;; 	| new_lyrics ADDLYRICS {
;; 		PARSER->lexer_->push_lyric_state ();
;; 	} grouped_music_list {
;; 		PARSER->lexer_->pop_state ();
;; 		$$ = scm_cons ($4, $1);
;; 	}
;; 	;

;; re_rhythmed_music:
;; 	grouped_music_list new_lyrics {
;; 		$$ = MAKE_SYNTAX ("add-lyrics", @$, $1, scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	| LYRICSTO simple_string {
;; 		PARSER->lexer_->push_lyric_state ();
;; 	} music {
;; 		PARSER->lexer_->pop_state ();
;; 		$$ = MAKE_SYNTAX ("lyric-combine", @$, $2, $4);
;; 	}
;; 	;

;; context_change:
;; 	CHANGE STRING '=' STRING  {
;; 		$$ = MAKE_SYNTAX ("context-change", @$, scm_string_to_symbol ($2), $4);
;; 	}
;; 	;


;; property_path_revved:
;; 	embedded_scm {
;; 		$$ = scm_cons ($1, SCM_EOL);
;; 	}
;; 	| property_path_revved embedded_scm {
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	;

;; property_path:
;; 	property_path_revved  {
;; 		$$ = scm_reverse_x ($1, SCM_EOL);
;; 	}
;; 	;

;; property_operation:
;; 	STRING '=' scalar {
;; 		$$ = scm_list_3 (ly_symbol2scm ("assign"),
;; 			scm_string_to_symbol ($1), $3);
;; 	}
;; 	| UNSET simple_string {
;; 		$$ = scm_list_2 (ly_symbol2scm ("unset"),
;; 			scm_string_to_symbol ($2));
;; 	}
;; 	| OVERRIDE simple_string property_path '=' scalar {
;; 		$$ = scm_append (scm_list_2 (scm_list_3 (ly_symbol2scm ("push"),
;; 							scm_string_to_symbol ($2), $5),
;; 					     $3));
;; 	}
;; 	| REVERT simple_string embedded_scm {
;; 		$$ = scm_list_3 (ly_symbol2scm ("pop"),
;; 			scm_string_to_symbol ($2), $3);
;; 	}
;; 	;

;; context_def_mod:
;; 	CONSISTS { $$ = ly_symbol2scm ("consists"); }
;; 	| REMOVE { $$ = ly_symbol2scm ("remove"); }

;; 	| ACCEPTS { $$ = ly_symbol2scm ("accepts"); }
;; 	| DEFAULTCHILD { $$ = ly_symbol2scm ("default-child"); }
;; 	| DENIES { $$ = ly_symbol2scm ("denies"); }

;; 	| ALIAS { $$ = ly_symbol2scm ("alias"); }
;; 	| TYPE { $$ = ly_symbol2scm ("translator-type"); }
;; 	| DESCRIPTION { $$ = ly_symbol2scm ("description"); }
;; 	| NAME { $$ = ly_symbol2scm ("context-name"); }
;; 	;

;; context_mod:
;; 	property_operation { $$ = $1; }
;; 	| context_def_mod STRING {
;; 		$$ = scm_list_2 ($1, $2);
;; 	}
;; 	| context_def_mod embedded_scm {
;; 	   if (ly_symbol2scm ("consists") != $1)
;; 	   {
;; 	     $$ = SCM_EOL;
;;              PARSER->parser_error (@1, _ ("only \\consists takes non-string argument."));
;; 	   }
;; 	   else
;; 	   {
;;  	     $$ = scm_list_2 ($1, $2);
;; 	   }
;; 	}
;; 	;

;; context_prop_spec:
;; 	simple_string {
;; 		if (!is_regular_identifier ($1))
;; 		{
;; 			@$.error (_("Grob name should be alphanumeric"));
;; 		}

;; 		$$ = scm_list_2 (ly_symbol2scm ("Bottom"),
;; 			scm_string_to_symbol ($1));
;; 	}
;; 	| simple_string '.' simple_string {
;; 		$$ = scm_list_2 (scm_string_to_symbol ($1),
;; 			scm_string_to_symbol ($3));
;; 	}
;; 	;

;; simple_music_property_def:
;; 	OVERRIDE context_prop_spec property_path '=' scalar {
;; 		$$ = scm_append (scm_list_2 (scm_list_n (scm_car ($2),
;; 				ly_symbol2scm ("OverrideProperty"),
;; 				scm_cadr ($2),
;; 				$5, SCM_UNDEFINED),
;; 				$3));
;; 	}
;; 	| REVERT context_prop_spec embedded_scm {
;; 		$$ = scm_list_4 (scm_car ($2),
;; 			ly_symbol2scm ("RevertProperty"),
;; 			scm_cadr ($2),
;; 			$3);
;; 	}
;; 	| SET context_prop_spec '=' scalar {
;; 		$$ = scm_list_4 (scm_car ($2),
;; 			ly_symbol2scm ("PropertySet"),
;; 			scm_cadr ($2),
;; 			$4);
;; 	}
;; 	| UNSET context_prop_spec {
;; 		$$ = scm_list_3 (scm_car ($2),
;; 			ly_symbol2scm ("PropertyUnset"),
;; 			scm_cadr ($2));
;; 	}
;; 	;

;; music_property_def:
;; 	simple_music_property_def {
;; 		$$ = LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant ("property-operation"), scm_cons (PARSER->self_scm (), scm_cons2 (make_input (@$), SCM_BOOL_F, $1)));
;; 	}
;; 	| ONCE simple_music_property_def {
;; 		$$ = LOWLEVEL_MAKE_SYNTAX (ly_lily_module_constant ("property-operation"), scm_cons (PARSER->self_scm (), scm_cons2 (make_input (@$), SCM_BOOL_T, $2)));
;; 	}
;; 	;

;; string:
;; 	STRING {
;; 		$$ = $1;
;; 	}
;; 	| STRING_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| string '+' string {
;; 		$$ = scm_string_append (scm_list_2 ($1, $3));
;; 	}
;; 	;

;; simple_string: STRING {
;; 		$$ = $1;
;; 	}
;; 	| LYRICS_STRING {
;; 		$$ = $1;
;; 	}
;; 	| STRING_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	;

;; scalar: string {
;; 		$$ = $1;
;; 	}
;; 	| LYRICS_STRING {
;; 		$$ = $1;
;; 	}
;; 	| bare_number {
;; 		$$ = $1;
;; 	}
;;         | embedded_scm {
;; 		$$ = $1;
;; 	}
;; 	| full_markup {
;; 		$$ = $1;
;; 	}
;; 	| DIGIT {
;; 		$$ = scm_from_int ($1);
;; 	}
;; 	;

;; event_chord:
;; 	/* TODO: Create a special case that avoids the creation of
;; 	   EventChords around simple_elements that have no post_events?
;; 	 */
;; 	simple_chord_elements post_events	{
;; 		SCM elts = ly_append2 ($1, scm_reverse_x ($2, SCM_EOL));

;; 		Input i;
;; 		/* why is this giving wrong start location? -ns
;; 		 * i = @$; */
;; 		i.set_location (@1, @2);
;; 		$$ = MAKE_SYNTAX ("event-chord", i, elts);
;; 	}
;; 	| CHORD_REPETITION optional_notemode_duration post_events {
;; 		Input i;
;; 		i.set_location (@1, @3);
;; 		$$ = MAKE_SYNTAX ("repetition-chord", i,
;; 				  PARSER->lexer_->chord_repetition_.last_chord_,
;; 				  PARSER->lexer_->chord_repetition_.repetition_function_,
;; 				  $2, scm_reverse_x ($3, SCM_EOL));
;; 	}
;; 	| MULTI_MEASURE_REST optional_notemode_duration post_events {
;; 		Input i;
;; 		i.set_location (@1, @3);
;; 		$$ = MAKE_SYNTAX ("multi-measure-rest", i, $2,
;; 				  scm_reverse_x ($3, SCM_EOL));
;; 	}
;; 	| command_element
;; 	/* note chord elements are memorized into
;; 	   PARSER->lexer_->chord_repetition_ so that the chord repetition
;; 	   mechanism copy them when a chord repetition symbol is found
;; 	*/
;; 	| note_chord_element	{
;; 		PARSER->lexer_->chord_repetition_.last_chord_ = $$;
;; 	}
;; 	;


;; note_chord_element:
;; 	chord_body optional_notemode_duration post_events
;; 	{
;; 		Music *m = unsmob_music ($1);
;; 		SCM dur = unsmob_duration ($2)->smobbed_copy ();
;; 		SCM es = m->get_property ("elements");
;; 		SCM postevs = scm_reverse_x ($3, SCM_EOL);

;; 		for (SCM s = es; scm_is_pair (s); s = scm_cdr (s))
;; 		  unsmob_music (scm_car (s))->set_property ("duration", dur);
;; 		es = ly_append2 (es, postevs);

;; 		m-> set_property ("elements", es);
;; 		m->set_spot (@$);
;; 		$$ = m->self_scm ();
;; 	}
;; 	;

;; chord_body:
;; 	ANGLE_OPEN chord_body_elements ANGLE_CLOSE
;; 	{
;; 		$$ = MAKE_SYNTAX ("event-chord", @$, scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	;

;; chord_body_elements:
;; 	/* empty */ 		{ $$ = SCM_EOL; }
;; 	| chord_body_elements chord_body_element {
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	;

;; chord_body_element:
;; 	pitch exclamations questions octave_check post_events
;; 	{
;; 		int q = $3;
;; 		int ex = $2;
;; 		SCM check = $4;
;; 		SCM post = $5;

;; 		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
;; 		n->set_property ("pitch", $1);
;; 		if (q % 2)
;; 			n->set_property ("cautionary", SCM_BOOL_T);
;; 		if (ex % 2 || q % 2)
;; 			n->set_property ("force-accidental", SCM_BOOL_T);

;; 		if (scm_is_pair (post)) {
;; 			SCM arts = scm_reverse_x (post, SCM_EOL);
;; 			n->set_property ("articulations", arts);
;; 		}
;; 		if (scm_is_number (check))
;; 		{
;; 			int q = scm_to_int (check);
;; 			n->set_property ("absolute-octave", scm_from_int (q-1));
;; 		}

;; 		$$ = n->unprotect ();
;; 	}
;; 	| DRUM_PITCH post_events {
;; 		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
;; 		n->set_property ("duration", $2);
;; 		n->set_property ("drum-type", $1);

;; 		if (scm_is_pair ($2)) {
;; 			SCM arts = scm_reverse_x ($2, SCM_EOL);
;; 			n->set_property ("articulations", arts);
;; 		}
;; 		$$ = n->unprotect ();
;; 	}
;; 	| music_function_chord_body {
;; 		$$ = run_music_function (PARSER, $1);
;; 	}
;; 	;

;; music_function_identifier_musicless_prefix: MUSIC_FUNCTION {
;; 		SCM sig = scm_object_property (yylval.scm, ly_symbol2scm ("music-function-signature"));
;; 		if (scm_is_pair (sig) && to_boolean (scm_memq (ly_music_p_proc, scm_cdr (scm_reverse (sig)))))
;; 		{
;; 			PARSER->parser_error (@$, "Music function applied to event may not have a Music argument, except as the last argument.");
;; 		}
;; 	}
;; 	;

;; music_function_chord_body:
;; 	/* We could allow chord functions to have multiple music arguments,
;; 	   but it's more consistent with music_function_event if we
;; 	   prohibit it here too */
;; 	music_function_identifier_musicless_prefix EXPECT_MUSIC function_arglist_nonmusic chord_body_element {
;; 		$$ = ly_append2 (scm_list_2 ($1, make_input (@$)), scm_reverse_x ($3, scm_list_1 ($4)));
;; 	}
;; 	| music_function_identifier_musicless_prefix function_arglist_nonmusic {
;; 		$$ = ly_append2 (scm_list_2 ($1, make_input (@$)), scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	;

;; music_function_event:
;; 	/* Post-events can only have the last argument as music, without this
;; 	   restriction we get a shift/reduce conflict from e.g.
;; 	   c8-\partcombine c8 -. */
;; 	music_function_identifier_musicless_prefix EXPECT_MUSIC function_arglist_nonmusic post_event {
;; 		$$ = ly_append2 (scm_list_2 ($1, make_input (@$)), scm_reverse_x ($3, scm_list_1 ($4)));
;; 	}
;; 	| music_function_identifier_musicless_prefix function_arglist_nonmusic {
;; 		$$ = ly_append2 (scm_list_2 ($1, make_input (@$)), scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	;

;; command_element:
;; 	command_event {
;; 		$$ = $1;
;; 	}
;; 	| SKIP duration_length {
;; 		$$ = MAKE_SYNTAX ("skip-music", @$, $2);
;; 	}
;; 	| E_BRACKET_OPEN {
;; 		Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$);
;; 		m->set_property ("span-direction", scm_from_int (START));
;; 		$$ = m->unprotect();
;; 	}
;; 	| E_BRACKET_CLOSE {
;; 		Music *m = MY_MAKE_MUSIC ("LigatureEvent", @$);
;; 		m->set_property ("span-direction", scm_from_int (STOP));
;; 		$$ = m->unprotect ();
;; 	}
;; 	| E_BACKSLASH {
;; 		$$ = MAKE_SYNTAX ("voice-separator", @$, SCM_UNDEFINED);
;; 	}
;; 	| '|'      {
;; 		SCM pipe = PARSER->lexer_->lookup_identifier ("pipeSymbol");

;; 		Music *m = unsmob_music (pipe);
;; 		if (m)
;; 		{
;; 			m = m->clone ();
;; 			m->set_spot (@$);
;; 			$$ = m->unprotect ();
;; 		}
;; 		else
;; 			$$ = MAKE_SYNTAX ("bar-check", @$, SCM_UNDEFINED);

;; 	}
;; 	| PARTIAL duration_length	{
;; 		Moment m = - unsmob_duration ($2)->get_length ();
;; 		$$ = MAKE_SYNTAX ("property-operation", @$, SCM_BOOL_F, ly_symbol2scm ("Timing"), ly_symbol2scm ("PropertySet"), ly_symbol2scm ("measurePosition"), m.smobbed_copy ());
;; 		$$ = MAKE_SYNTAX ("context-specification", @$, ly_symbol2scm ("Score"), SCM_BOOL_F, $$, SCM_EOL, SCM_BOOL_F);
;; 	}

;; 	| TIME_T fraction  {
;; 		SCM proc = ly_lily_module_constant ("make-time-signature-set");

;; 		$$ = scm_apply_2   (proc, scm_car ($2), scm_cdr ($2), SCM_EOL);
;; 	}
;; 	| MARK scalar {
;; 		SCM proc = ly_lily_module_constant ("make-mark-set");

;; 		$$ = scm_call_1 (proc, $2);
;; 	}
;; 	;

;; command_event:
;; 	E_TILDE {
;; 		$$ = MY_MAKE_MUSIC ("PesOrFlexaEvent", @$)->unprotect ();
;; 	}
;; 	| MARK DEFAULT  {
;; 		Music *m = MY_MAKE_MUSIC ("MarkEvent", @$);
;; 		$$ = m->unprotect ();
;; 	}
;; 	| tempo_event {
;; 		$$ = $1;
;; 	}
;; 	| KEY DEFAULT {
;; 		Music *key = MY_MAKE_MUSIC ("KeyChangeEvent", @$);
;; 		$$ = key->unprotect ();
;; 	}
;; 	| KEY NOTENAME_PITCH SCM_IDENTIFIER 	{

;; 		Music *key = MY_MAKE_MUSIC ("KeyChangeEvent", @$);
;; 		if (scm_ilength ($3) > 0)
;; 		{
;; 			key->set_property ("pitch-alist", $3);
;; 			key->set_property ("tonic", Pitch (0, 0, 0).smobbed_copy ());
;; 			key->transpose (* unsmob_pitch ($2));
;; 		} else {
;; 			PARSER->parser_error (@3, _ ("second argument must be pitch list"));
;; 		}

;; 		$$ = key->unprotect ();
;; 	}
;; 	;


;; post_events:
;; 	/* empty */ {
;; 		$$ = SCM_EOL;
;; 	}
;; 	| post_events post_event {
;; 		unsmob_music ($2)->set_spot (@2);
;; 		$$ = scm_cons ($2, $$);
;; 	}
;; 	;

;; post_event:
;; 	direction_less_event {
;; 		$$ = $1;
;; 	}
;; 	| '-' music_function_event {
;; 		$$ = run_music_function (PARSER, $2);
;; 	}
;; 	| HYPHEN {
;; 		if (!PARSER->lexer_->is_lyric_state ())
;; 			PARSER->parser_error (@1, _ ("have to be in Lyric mode for lyrics"));
;; 		$$ = MY_MAKE_MUSIC ("HyphenEvent", @$)->unprotect ();
;; 	}
;; 	| EXTENDER {
;; 		if (!PARSER->lexer_->is_lyric_state ())
;; 			PARSER->parser_error (@1, _ ("have to be in Lyric mode for lyrics"));
;; 		$$ = MY_MAKE_MUSIC ("ExtenderEvent", @$)->unprotect ();
;; 	}
;; 	| script_dir direction_reqd_event {
;; 		if ($1)
;; 		{
;; 			Music *m = unsmob_music ($2);
;; 			m->set_property ("direction", scm_from_int ($1));
;; 		}
;; 		$$ = $2;
;; 	}
;; 	| script_dir direction_less_event {
;; 		if ($1)
;; 		{
;; 			Music *m = unsmob_music ($2);
;; 			m->set_property ("direction", scm_from_int ($1));
;; 		}
;; 		$$ = $2;
;; 	}
;; 	| string_number_event
;; 	;

;; string_number_event:
;; 	E_UNSIGNED {
;; 		Music *s = MY_MAKE_MUSIC ("StringNumberEvent", @$);
;; 		s->set_property ("string-number", scm_from_int ($1));
;; 		$$ = s->unprotect ();
;; 	}
;; 	;

;; direction_less_char:
;; 	'['  {
;; 		$$ = ly_symbol2scm ("bracketOpenSymbol");
;; 	}
;; 	| ']'  {
;; 		$$ = ly_symbol2scm ("bracketCloseSymbol");
;; 	}
;; 	| '~'  {
;; 		$$ = ly_symbol2scm ("tildeSymbol");
;; 	}
;; 	| '('  {
;; 		$$ = ly_symbol2scm ("parenthesisOpenSymbol");
;; 	}
;; 	| ')'  {
;; 		$$ = ly_symbol2scm ("parenthesisCloseSymbol");
;; 	}
;; 	| E_EXCLAMATION  {
;; 		$$ = ly_symbol2scm ("escapedExclamationSymbol");
;; 	}
;; 	| E_OPEN  {
;; 		$$ = ly_symbol2scm ("escapedParenthesisOpenSymbol");
;; 	}
;; 	| E_CLOSE  {
;; 		$$ = ly_symbol2scm ("escapedParenthesisCloseSymbol");
;; 	}
;; 	| E_ANGLE_CLOSE  {
;; 		$$ = ly_symbol2scm ("escapedBiggerSymbol");
;; 	}
;; 	| E_ANGLE_OPEN  {
;; 		$$ = ly_symbol2scm ("escapedSmallerSymbol");
;; 	}
;; 	;

;; direction_less_event:
;; 	direction_less_char {
;; 		SCM predefd = PARSER->lexer_->lookup_identifier_symbol ($1);
;; 		Music *m = 0;
;; 		if (unsmob_music (predefd))
;; 		{
;; 			m = unsmob_music (predefd)->clone ();
;; 			m->set_spot (@$);
;; 		}
;; 		else
;; 		{
;; 			m = MY_MAKE_MUSIC ("Music", @$);
;; 		}
;; 		$$ = m->unprotect ();
;; 	}
;; 	| EVENT_IDENTIFIER	{
;; 		$$ = $1;
;; 	}
;; 	| tremolo_type  {
;;                Music *a = MY_MAKE_MUSIC ("TremoloEvent", @$);
;;                a->set_property ("tremolo-type", scm_from_int ($1));
;;                $$ = a->unprotect ();
;;         }
;; 	;

;; direction_reqd_event:
;; 	gen_text_def {
;; 		$$ = $1;
;; 	}
;; 	| script_abbreviation {
;; 		SCM s = PARSER->lexer_->lookup_identifier ("dash" + ly_scm2string ($1));
;; 		Music *a = MY_MAKE_MUSIC ("ArticulationEvent", @$);
;; 		if (scm_is_string (s))
;; 			a->set_property ("articulation-type", s);
;; 		else PARSER->parser_error (@1, _ ("expecting string as script definition"));
;; 		$$ = a->unprotect ();
;; 	}
;; 	;

;; octave_check:
;; 	/**/ { $$ = SCM_EOL; }
;; 	| '='  { $$ = scm_from_int (0); }
;; 	| '=' sub_quotes { $$ = scm_from_int (-$2); }
;; 	| '=' sup_quotes { $$ = scm_from_int ($2); }
;; 	;

;; sup_quotes:
;; 	'\'' {
;; 		$$ = 1;
;; 	}
;; 	| sup_quotes '\'' {
;; 		$$ ++;
;; 	}
;; 	;

;; sub_quotes:
;; 	',' {
;; 		$$ = 1;
;; 	}
;; 	| sub_quotes ',' {
;; 		$$++;
;; 	}
;; 	;

;; steno_pitch:
;; 	NOTENAME_PITCH	{
;; 		$$ = $1;
;; 	}
;; 	| NOTENAME_PITCH sup_quotes 	{
;; 		Pitch p = *unsmob_pitch ($1);
;; 		p = p.transposed (Pitch ($2,0,0));
;; 		$$ = p.smobbed_copy ();
;; 	}
;; 	| NOTENAME_PITCH sub_quotes	 {
;; 		Pitch p =* unsmob_pitch ($1);
;; 		p = p.transposed (Pitch (-$2,0,0));
;; 		$$ = p.smobbed_copy ();
;; 	}
;; 	;

;; /*
;; ugh. duplication
;; */

;; steno_tonic_pitch:
;; 	TONICNAME_PITCH	{
;; 		$$ = $1;
;; 	}
;; 	| TONICNAME_PITCH sup_quotes 	{
;; 		Pitch p = *unsmob_pitch ($1);
;; 		p = p.transposed (Pitch ($2,0,0));
;; 		$$ = p.smobbed_copy ();
;; 	}
;; 	| TONICNAME_PITCH sub_quotes	 {
;; 		Pitch p = *unsmob_pitch ($1);

;; 		p = p.transposed (Pitch (-$2,0,0));
;; 		$$ = p.smobbed_copy ();
;; 	}
;; 	;

;; pitch:
;; 	steno_pitch {
;; 		$$ = $1;
;; 	}
;; 	;

;; pitch_also_in_chords:
;; 	pitch
;; 	| steno_tonic_pitch
;; 	;

;; gen_text_def:
;; 	full_markup {
;; 		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
;; 		t->set_property ("text", $1);
;; 		$$ = t->unprotect ();
;; 	}
;; 	| string {
;; 		Music *t = MY_MAKE_MUSIC ("TextScriptEvent", @$);
;; 		t->set_property ("text",
;; 			make_simple_markup ($1));
;; 		$$ = t->unprotect ();
;; 	}
;; 	| DIGIT {
;; 		Music *t = MY_MAKE_MUSIC ("FingeringEvent", @$);
;; 		t->set_property ("digit", scm_from_int ($1));
;; 		$$ = t->unprotect ();
;; 	}
;; 	;

;; script_abbreviation:
;; 	'^'		{
;; 		$$ = scm_from_locale_string ("Hat");
;; 	}
;; 	| '+'		{
;; 		$$ = scm_from_locale_string ("Plus");
;; 	}
;; 	| '-' 		{
;; 		$$ = scm_from_locale_string ("Dash");
;; 	}
;;  	| '|'		{
;; 		$$ = scm_from_locale_string ("Bar");
;; 	}
;; 	| ANGLE_CLOSE	{
;; 		$$ = scm_from_locale_string ("Larger");
;; 	}
;; 	| '.' 		{
;; 		$$ = scm_from_locale_string ("Dot");
;; 	}
;; 	| '_' {
;; 		$$ = scm_from_locale_string ("Underscore");
;; 	}
;; 	;

;; script_dir:
;; 	'_'	{ $$ = DOWN; }
;; 	| '^'	{ $$ = UP; }
;; 	| '-'	{ $$ = CENTER; }
;; 	;


;; absolute_pitch:
;; 	steno_pitch	{
;; 		$$ = $1;
;; 	}
;; 	;

;; duration_length:
;; 	multiplied_duration {
;; 		$$ = $1;
;; 	}
;; 	;

;; optional_notemode_duration:
;; 	{
;; 		Duration dd = PARSER->default_duration_;
;; 		$$ = dd.smobbed_copy ();
;; 	}
;; 	| multiplied_duration	{
;; 		$$ = $1;
;; 		PARSER->default_duration_ = *unsmob_duration ($$);
;; 	}
;; 	;

;; steno_duration:
;; 	bare_unsigned dots		{
;; 		int len = 0;
;; 		if (!is_duration ($1))
;; 			PARSER->parser_error (@1, _f ("not a duration: %d", $1));
;; 		else
;; 			len = intlog2 ($1);

;; 		$$ = Duration (len, $2).smobbed_copy ();
;; 	}
;; 	| DURATION_IDENTIFIER dots	{
;; 		Duration *d = unsmob_duration ($1);
;; 		Duration k (d->duration_log (), d->dot_count () + $2);
;; 		k = k.compressed (d->factor ());
;; 		*d = k;
;; 		$$ = $1;
;; 	}
;; 	;

;; multiplied_duration:
;; 	steno_duration {
;; 		$$ = $1;
;; 	}
;; 	| multiplied_duration '*' bare_unsigned {
;; 		$$ = unsmob_duration ($$)->compressed ( $3) .smobbed_copy ();
;; 	}
;; 	| multiplied_duration '*' FRACTION {
;; 		Rational  m (scm_to_int (scm_car ($3)), scm_to_int (scm_cdr ($3)));

;; 		$$ = unsmob_duration ($$)->compressed (m).smobbed_copy ();
;; 	}
;; 	;

;; fraction:
;; 	FRACTION { $$ = $1; }
;; 	| UNSIGNED '/' UNSIGNED {
;; 		$$ = scm_cons (scm_from_int ($1), scm_from_int ($3));
;; 	}
;; 	;

;; dots:
;; 	/* empty */ 	{
;; 		$$ = 0;
;; 	}
;; 	| dots '.' {
;; 		$$ ++;
;; 	}
;; 	;

;; tremolo_type:
;; 	':'	{
;; 		$$ = 0;
;; 	}
;; 	| ':' bare_unsigned {
;; 		if (!is_duration ($2))
;; 			PARSER->parser_error (@2, _f ("not a duration: %d", $2));
;; 		$$ = $2;
;; 	}
;; 	;

;; bass_number:
;; 	DIGIT   {
;; 		$$ = scm_from_int ($1);
;; 	}
;; 	| UNSIGNED {
;; 		$$ = scm_from_int ($1);
;; 	}
;; 	| STRING { $$ = $1; }
;; 	| full_markup { $$ = $1; }
;; 	;

;; figured_bass_alteration:
;; 	'-' 	{ $$ = ly_rational2scm (FLAT_ALTERATION); }
;; 	| '+'	{ $$ = ly_rational2scm (SHARP_ALTERATION); }
;; 	| '!'	{ $$ = scm_from_int (0); }
;; 	;

;; bass_figure:
;; 	FIGURE_SPACE {
;; 		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
;; 		$$ = bfr->unprotect ();
;; 	}
;; 	| bass_number  {
;; 		Music *bfr = MY_MAKE_MUSIC ("BassFigureEvent", @$);
;; 		$$ = bfr->self_scm ();

;; 		if (scm_is_number ($1))
;; 			bfr->set_property ("figure", $1);
;; 		else if (Text_interface::is_markup ($1))
;; 			bfr->set_property ("text", $1);

;; 		bfr->unprotect ();
;; 	}
;; 	| bass_figure ']' {
;; 		$$ = $1;
;; 		unsmob_music ($1)->set_property ("bracket-stop", SCM_BOOL_T);
;; 	}
;; 	| bass_figure figured_bass_alteration {
;; 		Music *m = unsmob_music ($1);
;; 		if (scm_to_double ($2)) {
;; 			SCM salter = m->get_property ("alteration");
;; 			SCM alter = scm_is_number (salter) ? salter : scm_from_int (0);
;; 			m->set_property ("alteration",
;; 					 scm_sum (alter, $2));
;; 		} else {
;; 			m->set_property ("alteration", scm_from_int (0));
;; 		}
;; 	}
;; 	| bass_figure figured_bass_modification  {
;; 		Music *m = unsmob_music ($1);
;; 		if ($2 == ly_symbol2scm ("plus"))
;; 			{
;; 			m->set_property ("augmented", SCM_BOOL_T);
;; 			}
;; 		else if ($2 == ly_symbol2scm ("slash"))
;; 			{
;; 			m->set_property ("diminished", SCM_BOOL_T);
;; 			}
;; 		else if ($2 == ly_symbol2scm ("exclamation"))
;; 			{
;; 			m->set_property ("no-continuation", SCM_BOOL_T);
;; 			}
;; 		else if ($2 == ly_symbol2scm ("backslash"))
;; 			{
;; 			m->set_property ("augmented-slash", SCM_BOOL_T);
;; 			}
;; 	}
;; 	;


;; figured_bass_modification:
;; 	E_PLUS		{
;; 		$$ = ly_symbol2scm ("plus");
;; 	}
;; 	| E_EXCLAMATION {
;; 		$$ = ly_symbol2scm ("exclamation");
;; 	}
;; 	| '/'		{
;; 		$$ = ly_symbol2scm ("slash");
;; 	}
;; 	| E_BACKSLASH {
;; 		$$ = ly_symbol2scm ("backslash");
;; 	}
;; 	;

;; br_bass_figure:
;; 	bass_figure {
;; 		$$ = $1;
;; 	}
;; 	| '[' bass_figure {
;; 		$$ = $2;
;; 		unsmob_music ($$)->set_property ("bracket-start", SCM_BOOL_T);
;; 	}
;; 	;

;; figure_list:
;; 	/**/		{
;; 		$$ = SCM_EOL;
;; 	}
;; 	| figure_list br_bass_figure {
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	;

;; figure_spec:
;; 	FIGURE_OPEN figure_list FIGURE_CLOSE {
;; 		$$ = scm_reverse_x ($2, SCM_EOL);
;; 	}
;; 	;


;; optional_rest:
;; 	/**/   { $$ = 0; }
;; 	| REST { $$ = 1; }
;; 	;

;; simple_element:
;; 	pitch exclamations questions octave_check optional_notemode_duration optional_rest {
;; 		if (!PARSER->lexer_->is_note_state ())
;; 			PARSER->parser_error (@1, _ ("have to be in Note mode for notes"));

;; 		Music *n = 0;
;; 		if ($6)
;; 			n = MY_MAKE_MUSIC ("RestEvent", @$);
;; 		else
;; 			n = MY_MAKE_MUSIC ("NoteEvent", @$);

;; 		n->set_property ("pitch", $1);
;; 		n->set_property ("duration", $5);

;; 		if (scm_is_number ($4))
;; 		{
;; 			int q = scm_to_int ($4);
;; 			n->set_property ("absolute-octave", scm_from_int (q-1));
;; 		}

;; 		if ($3 % 2)
;; 			n->set_property ("cautionary", SCM_BOOL_T);
;; 		if ($2 % 2 || $3 % 2)
;; 			n->set_property ("force-accidental", SCM_BOOL_T);

;; 		$$ = n->unprotect ();
;; 	}
;; 	| DRUM_PITCH optional_notemode_duration {
;; 		Music *n = MY_MAKE_MUSIC ("NoteEvent", @$);
;; 		n->set_property ("duration", $2);
;; 		n->set_property ("drum-type", $1);

;; 		$$ = n->unprotect ();
;; 	}
;;  	| RESTNAME optional_notemode_duration		{
;; 		Music *ev = 0;
;;  		if (ly_scm2string ($1) == "s") {
;; 			/* Space */
;; 			ev = MY_MAKE_MUSIC ("SkipEvent", @$);
;; 		  }
;; 		else {
;; 			ev = MY_MAKE_MUSIC ("RestEvent", @$);

;; 		    }
;; 		ev->set_property ("duration", $2);
;;  		$$ = ev->unprotect ();
;; 	}
;; 	| lyric_element optional_notemode_duration 	{
;; 		if (!PARSER->lexer_->is_lyric_state ())
;; 			PARSER->parser_error (@1, _ ("have to be in Lyric mode for lyrics"));

;; 		Music *levent = MY_MAKE_MUSIC ("LyricEvent", @$);
;; 		levent->set_property ("text", $1);
;; 		levent->set_property ("duration",$2);
;; 		$$= levent->unprotect ();
;; 	}
;; 	;

;; simple_chord_elements:
;; 	simple_element	{
;; 		$$ = scm_list_1 ($1);
;; 	}
;; 	| new_chord {
;;                 if (!PARSER->lexer_->is_chord_state ())
;;                         PARSER->parser_error (@1, _ ("have to be in Chord mode for chords"));
;;                 $$ = $1;
;; 	}
;; 	| figure_spec optional_notemode_duration {
;; 		for (SCM s = $1; scm_is_pair (s); s = scm_cdr (s))
;; 		{
;; 			unsmob_music (scm_car (s))->set_property ("duration", $2);
;; 		}
;; 		$$ = $1;
;; 	}
;; 	;

;; lyric_element:
;; 	lyric_markup {
;; 		$$ = $1;
;; 	}
;; 	| LYRICS_STRING {
;; 		$$ = $1;
;; 	}
;; 	;

;; new_chord:
;; 	steno_tonic_pitch optional_notemode_duration   {
;; 		$$ = make_chord_elements ($1, $2, SCM_EOL);
;; 	}
;; 	| steno_tonic_pitch optional_notemode_duration chord_separator chord_items {
;; 		SCM its = scm_reverse_x ($4, SCM_EOL);
;; 		$$ = make_chord_elements ($1, $2, scm_cons ($3, its));
;; 	}
;; 	;

;; chord_items:
;; 	/**/ {
;; 		$$ = SCM_EOL;
;; 	}
;; 	| chord_items chord_item {
;; 		$$ = scm_cons ($2, $$);
;; 	}
;; 	;

;; chord_separator:
;; 	CHORD_COLON {
;; 		$$ = ly_symbol2scm ("chord-colon");
;; 	}
;; 	| CHORD_CARET {
;; 		$$ = ly_symbol2scm ("chord-caret");
;; 	}
;; 	| CHORD_SLASH steno_tonic_pitch {
;;  		$$ = scm_list_2 (ly_symbol2scm ("chord-slash"), $2);
;; 	}
;; 	| CHORD_BASS steno_tonic_pitch {
;; 		$$ = scm_list_2 (ly_symbol2scm ("chord-bass"), $2);
;; 	}
;; 	;

;; chord_item:
;; 	chord_separator {
;; 		$$ = $1;
;; 	}
;; 	| step_numbers {
;; 		$$ = scm_reverse_x ($1, SCM_EOL);
;; 	}
;; 	| CHORD_MODIFIER  {
;; 		$$ = $1;
;; 	}
;; 	;

;; step_numbers:
;; 	step_number { $$ = scm_cons ($1, SCM_EOL); }
;; 	| step_numbers '.' step_number {
;; 		$$ = scm_cons ($3, $$);
;; 	}
;; 	;

;; step_number:
;; 	bare_unsigned {
;; 		$$ = make_chord_step ($1, 0);
;;         }
;; 	| bare_unsigned '+' {
;; 		$$ = make_chord_step ($1, SHARP_ALTERATION);
;; 	}
;; 	| bare_unsigned CHORD_MINUS {
;; 		$$ = make_chord_step ($1, FLAT_ALTERATION);
;; 	}
;; 	;

;; /*
;; 	UTILITIES

;; TODO: should deprecate in favor of Scheme?

;;  */
;; number_expression:
;; 	number_expression '+' number_term {
;; 		$$ = scm_sum ($1, $3);
;; 	}
;; 	| number_expression '-' number_term {
;; 		$$ = scm_difference ($1, $3);
;; 	}
;; 	| number_term
;; 	;

;; number_term:
;; 	number_factor {
;; 		$$ = $1;
;; 	}
;; 	| number_factor '*' number_factor {
;; 		$$ = scm_product ($1, $3);
;; 	}
;; 	| number_factor '/' number_factor {
;; 		$$ = scm_divide ($1, $3);
;; 	}
;; 	;

;; number_factor:
;; 	'-'  number_factor { /* %prec UNARY_MINUS */
;; 		$$ = scm_difference ($2, SCM_UNDEFINED);
;; 	}
;; 	| bare_number
;; 	;


;; bare_number:
;; 	UNSIGNED	{
;; 		$$ = scm_from_int ($1);
;; 	}
;; 	| REAL		{
;; 		$$ = $1;
;; 	}
;; 	| NUMBER_IDENTIFIER		{
;; 		$$ = $1;
;; 	}
;; 	| REAL NUMBER_IDENTIFIER	{
;; 		$$ = scm_from_double (scm_to_double ($1) *scm_to_double ($2));
;; 	}
;; 	| UNSIGNED NUMBER_IDENTIFIER	{
;; 		$$ = scm_from_double ($1 *scm_to_double ($2));
;; 	}
;; 	;


;; bare_unsigned:
;; 	UNSIGNED {
;; 			$$ = $1;
;; 	}
;; 	| DIGIT {
;; 		$$ = $1;
;; 	}
;; 	;

;; unsigned_number:
;; 	bare_unsigned  { $$ = scm_from_int ($1); }
;; 	| NUMBER_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	;


;; exclamations:
;; 		{ $$ = 0; }
;; 	| exclamations '!'	{ $$ ++; }
;; 	;

;; questions:
;; 		{ $$ = 0; }
;; 	| questions '?'	{ $$ ++; }
;; 	;

;; /*
;; This should be done more dynamically if possible.
;; */

;; lyric_markup:
;; 	LYRIC_MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| LYRIC_MARKUP
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_top {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	;

;; full_markup_list:
;; 	MARKUPLINES
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_list {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	;

;; full_markup:
;; 	MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| MARKUP
;; 		{ PARSER->lexer_->push_markup_state (); }
;; 	markup_top {
;; 		$$ = $3;
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	;

;; markup_top:
;; 	markup_list {
;; 		$$ = scm_list_2 (ly_lily_module_constant ("line-markup"),  $1);
;; 	}
;; 	| markup_head_1_list simple_markup	{
;; 		$$ = scm_car (scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, scm_list_1 ($2)));
;; 	}
;; 	| simple_markup	{
;; 		$$ = $1;
;; 	}
;; 	;

;; markup_list:
;; 	markup_composed_list {
;; 		$$ = $1;
;; 	}
;; 	| markup_braced_list {
;; 		$$ = $1;
;; 	}
;; 	| markup_command_list {
;; 		$$ = scm_list_1 ($1);
;; 	}
;; 	;

;; markup_composed_list:
;; 	markup_head_1_list markup_braced_list {
;; 		$$ = scm_call_2 (ly_lily_module_constant ("map-markup-command-list"), $1, $2);

;; 	}
;; 	;

;; markup_braced_list:
;; 	'{' markup_braced_list_body '}'	{
;; 		$$ = scm_reverse_x ($2, SCM_EOL);
;; 	}
;; 	;

;; markup_braced_list_body:
;; 	/* empty */	{  $$ = SCM_EOL; }
;; 	| markup_braced_list_body markup {
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	| markup_braced_list_body markup_list {
;; 		$$ = scm_reverse_x ($2, $1);
;; 	}
;; 	;

;; markup_command_list:
;; 	MARKUP_LIST_FUNCTION markup_command_list_arguments {
;; 	  $$ = scm_cons ($1, scm_reverse_x($2, SCM_EOL));
;; 	}
;; 	;

;; markup_command_basic_arguments:
;; 	EXPECT_MARKUP_LIST markup_command_list_arguments markup_list {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_SCM markup_command_list_arguments embedded_scm {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	| EXPECT_NO_MORE_ARGS {
;; 	  $$ = SCM_EOL;
;; 	}
;; 	;

;; markup_command_list_arguments:
;; 	markup_command_basic_arguments { $$ = $1; }
;; 	| EXPECT_MARKUP markup_command_list_arguments markup {
;; 	  $$ = scm_cons ($3, $2);
;; 	}
;; 	;

;; markup_head_1_item:
;; 	MARKUP_FUNCTION EXPECT_MARKUP markup_command_list_arguments {
;; 	  $$ = scm_cons ($1, scm_reverse_x ($3, SCM_EOL));
;; 	}
;; 	;

;; markup_head_1_list:
;; 	markup_head_1_item	{
;; 		$$ = scm_list_1 ($1);
;; 	}
;; 	| markup_head_1_list markup_head_1_item	{
;; 		$$ = scm_cons ($2, $1);
;; 	}
;; 	;

;; simple_markup:
;; 	STRING {
;; 		$$ = make_simple_markup ($1);
;; 	}
;; 	| MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| LYRIC_MARKUP_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| STRING_IDENTIFIER {
;; 		$$ = $1;
;; 	}
;; 	| SCORE {
;; 		SCM nn = PARSER->lexer_->lookup_identifier ("pitchnames");
;; 		PARSER->lexer_->push_note_state (alist_to_hashq (nn));
;; 	} '{' score_body '}' {
;; 		Score * sc = $4;
;; 		$$ = scm_list_2 (ly_lily_module_constant ("score-markup"), sc->self_scm ());
;; 		sc->unprotect ();
;; 		PARSER->lexer_->pop_state ();
;; 	}
;; 	| MARKUP_FUNCTION markup_command_basic_arguments {
;; 		$$ = scm_cons ($1, scm_reverse_x ($2, SCM_EOL));
;; 	}
;; 	;

;; markup:
;; 	markup_head_1_list simple_markup	{
;; 		SCM mapper = ly_lily_module_constant ("map-markup-command-list");
;; 		$$ = scm_car (scm_call_2 (mapper, $1, scm_list_1 ($2)));
;; 	}
;; 	| simple_markup	{
;; 		$$ = $1;
;; 	}
;; 	;




	
  )
)

; Just to get this out of my way... I don't wanted to make errors anyway! (real function later)
(define (displayerror arg1 arg2)
		(display arg1)
		(display arg2)(newline)
)


(lilypond-parser lexer displayerror)


;; Close input port
(close (current-input-port)) 
