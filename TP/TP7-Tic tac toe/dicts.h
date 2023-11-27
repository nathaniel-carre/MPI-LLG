struct List {
    int key;
    int val;
    struct List* next;
};

typedef struct List list;

struct Dict{
    int capacity;
    int size;
    list** data;
};

typedef struct Dict dict;

void list_free(list* lst);

list* constr(int k, int v, list* lst);

void dict_free(dict D);

dict create(void);

int size(dict D);

int hash(dict D, int k);

bool member_list(list* lst, int k);

bool member(dict D, int k);

int get(dict D, int k);

void resize(dict* D, int capa);

void add(dict* D, int k, int v);

void del(dict* D, int k);