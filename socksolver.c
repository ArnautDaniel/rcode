int atomContainedIn(int atom, int* temp){
    //Floating '\0',  we can do this I think,
    //since temp == len(ar)
    int i = 0;
    while(i+1){
        if (atom == *temp){
            return 1;
        }
        if (*temp == '\0'){
            *temp = atom;
            *(temp+1) = '\0';
            return 0;
        }
        temp++;
    }
    return 0;
}

int atomCountIn(int index, int atom, int* ar){
    if (index <= 0){
        return 0;
    }
    if (atom == *ar){
        return 1 + atomCountIn(index-1, atom, ar+1);
    }
    return atomCountIn(index-1, atom, ar+1);
}

int sockMerchantSolve(int index, int* ar,  int* temp, int atom){
    //End of ar
    if (index <= 0){
        return 0;
    }
    //If the current sock is already in temp than
    //return 0
    int result = atomContainedIn(atom, temp);
    if (result){
        return 0;
    }
    result = atomCountIn(index, atom, ar);
    return result / 2;
}

int sockMerchant(int n, int ar_count, int* ar) {
    int* temp = (int*) malloc (sizeof (int) * n + 1);
    *temp = '\0';
    int result;
    for (int i = 0; i < n+1; i++){
        result += sockMerchantSolve(n-i, ar, temp, *ar);
        ar++;
    }
    free(temp);
    return result;
}
