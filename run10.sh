for i in {1..2} ; do
    fn=$(printf /Users/Naomi/Desktop/git_clones/iba/gpkex-english/data/i_outdir/%04d.txt $i);\
    #echo ${fn}
    #stack exec run-gp
    stack exec extraction ${fn}
    stack exec evaluation
done