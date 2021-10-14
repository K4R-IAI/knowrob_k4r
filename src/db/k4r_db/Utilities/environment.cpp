#define CERT_TYPE "P12"
#define SANDBOX
#ifdef SANDBOX
    #define URL "https://dt-api.sandbox.knowledge4retail.org/k4r/api/v0/"
    #define GRAPHQL_URL "https://dt-api.sandbox.knowledge4retail.org/k4r/graphql"
    #define CERT_PATH "/src/db/k4r_db/K4R_DEV_CERTs/k4r-sandbox-client.p12"
    #define CERT_PASSWD "q7WgPL3OnopoyU4abkrw97LD3iqD"
    #define VERIFY_PEER false
#else
    #define URL "https://dt-api.dev.knowledge4retail.org/k4r/api/v0/"
    #define GRAPHQL_URL "https://dt-api.dev.knowledge4retail.org/k4r/graphql"
    #define CERT_PATH "/src/db/k4r_db/K4R_DEV_CERTs/k4r-dev-keystore.p12"
    #define CERT_PASSWD "8FdseHr0wCHwfuDmMv7QdKYWvnZg"
    #define VERIFY_PEER true
#endif