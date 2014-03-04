#ifndef CLASSES_H_
#define CLASSES_H_

struct mystruct {
    int x;
    int y;
};

class myclass {
public:
    myclass();
    virtual ~myclass();

    void method(int a, int b) const;
};

#endif /* CLASSES_H_ */
