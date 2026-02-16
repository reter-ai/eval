/*  _chibi_module.c -- Python module init for chibi_eval._chibi  */

#include <Python.h>
#include <chibi/eval.h>

/* Types defined in other files */
extern PyTypeObject ChibiContextType;
extern PyTypeObject ChibiSexpType;

static PyModuleDef chibi_module = {
    PyModuleDef_HEAD_INIT,
    .m_name = "_chibi",
    .m_doc = "C extension providing Eval language interpreter on chibi-scheme",
    .m_size = -1,
};

PyMODINIT_FUNC PyInit__chibi(void) {
    PyObject *m;

    /* Initialize types */
    if (PyType_Ready(&ChibiContextType) < 0)
        return NULL;
    if (PyType_Ready(&ChibiSexpType) < 0)
        return NULL;

    m = PyModule_Create(&chibi_module);
    if (m == NULL)
        return NULL;

    /* Add types to module */
    Py_INCREF(&ChibiContextType);
    if (PyModule_AddObject(m, "ChibiContext", (PyObject *)&ChibiContextType) < 0) {
        Py_DECREF(&ChibiContextType);
        Py_DECREF(m);
        return NULL;
    }

    Py_INCREF(&ChibiSexpType);
    if (PyModule_AddObject(m, "ChibiSexp", (PyObject *)&ChibiSexpType) < 0) {
        Py_DECREF(&ChibiSexpType);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}
