CFLAGS = -o -g
SRC    = src
TEST_DIR = tests
TEST_BINS = $(TEST_DIR)/bin
BUILD_DIR = build
SRCS   = $(wildcard $(SRC)/*.c)
BINS   = bin
TESTS  = $(wildcard $(TEST_DIR)/*.c)
OBJS   = $(patsubst $(SRC)/%.c, $(BUILD_DIR)/%.o, $(SRCS))

$(BUILD_DIR)/%.o: $(SRC)/%.c
	gcc -c $< -o $@ -g

compile: $(OBJS)

main: $(BUILD_DIR)/*.o
	gcc $^ -o $(BINS)/$@ -g

test_ct: $(BUILD_DIR)/hash_table.o $(TEST_DIR)/test_ct.c
	gcc $^ -o $(TEST_BINS)/$@ -lcriterion
	./$(TEST_BINS)/test_ct

test_tc: $(BUILD_DIR)/llist.o $(BUILD_DIR)/scope.o $(BUILD_DIR)/symbol_table.o $(BUILD_DIR)/token.o $(BUILD_DIR)/parser.o $(BUILD_DIR)/semantics.o $(BUILD_DIR)/lexer.o $(BUILD_DIR)/hash_table.o $(TEST_DIR)/test_tc.c
	gcc $^ -o $(TEST_BINS)/$@ -lcriterion
	./$(TEST_BINS)/test_tc

clear_objs:
	rm -rf $(BUILD_DIR)/*

clear_bins:
	rm -rf $(BINS)/*


