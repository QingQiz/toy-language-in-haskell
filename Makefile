obj_name = cpr
main_module = Main.hs
src_dir = ./src
src_file = $(shell echo $(src_dir)/*.hs)

$(obj_name) : $(main_module) $(src_file)
	ghc -O2 -j2 -i$(src_dir) $(main_module) -o $(obj_name)

test : $(obj_name)
	@echo "\ntest for ./test/t1_p.c0"
	@./$(obj_name) --ast ./test/t1_p.c0
	@echo "\ntest for ./test/t2_e.c0"
	@./$(obj_name) --ast ./test/t2_e.c0

clean:
	@rm -f ./*.hi ./*.o
	@rm -f $(src_dir)/*.hi $(src_dir)/*.o
	@rm -f $(obj_name)
