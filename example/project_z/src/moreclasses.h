#ifndef MORECLASSES_H_
#define MORECLASSES_H_

struct moremystruct {
    int x;
    int y;
};

class moremyclass {
public:
    moremyclass();
    virtual ~moremyclass();
    
    void anothermethod(int a, int b) const;
}; 

#endif /* MORECLASSES_H_ */
