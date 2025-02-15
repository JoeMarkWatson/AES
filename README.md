# AES

All current file versions should end in “_d2”

*Real data*

Joe working on real data
 - translate_real_essays_d (not in repo) - translates data to English, divides data into train and test 
 - validate_real_closed_d2.R - takes the training set, transforms to binary, carries out item purification on closed items, and saves model parameters
 - OUTDATED score_essays_GPT_d2.py - scoring essays only using GPT
 - OUTDATED score_essays_GPT_prompt2_d2.py - scoring essays only using GPT (alternate prompt)
 - OUTDATED score_essays_Gemini_d2.py - scoring essays only using Google Gemini
 - OUTDATED score_essays_Gemini_prompt2_d2.py - scoring essays only using Google Gemini (alternate prompt)
 - score_sents_GPT_d2.py - scoring all qual data using varying prompts
 - score_sents_GPT_prompt2_d2.py - scoring all qual data using varying prompts (alternate prompts)
 - score_sents_GPT_commPrompt_d2.py - scoring all qual data using the same evidence prompt
 - score_sents_GPT_commPromptCompare_d2.py - scoring all qual data using the same comparison prompt
 - est_LLM_single_sents_d2UPDATED.R - takes various GPT scores and uses each individually within an IRT model, creates various models using combinations of GPT scores as items, saves models
 - test_script_d2UPDATED.R - running simulation based on models created in est_LLM_single_sents_d2UPDATED.R

*Synth data*
Ivan working on synth data
 - gen_synth_closed_d2.R - generates synth closed responses
 - gen_synth_qual_responses_d2.py - generates synth essay and sentence-completion responses

Ivan currently working on generating synth essay and sentence-completion responses.
After that, we just use the exact same process that we used for the real data, except for the sim which also inspects distance from true theta

