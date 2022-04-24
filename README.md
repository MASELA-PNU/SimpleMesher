# Project: SimpleMesher



## Description

The **SimpleMesher** is designed to generate the surface mesh of simple shape for the seakeeping analysis. 



## License

The EaFort source code and associated files are distributed under a permissive free software license (BSD-3 clause). Please check the license files for details.

## Dependencies

* **EaFort** : 

  

## How to Install

### Pre-installation

* **EaFort**: https://gitlab.com/MASELA/EaFort (See ReadMe.md)



### Installation

* Path to library **EaFort**: */home/user/Codes/EaFort*, 

#### Debian/Ubuntu

```bash
cmake -S. -Bbuild -DPATH_EaFort=/home/user/Codes/EaFort
cmake --build build
```

**Verified:**

- Debian 10 (buster), gfortran v8.3.0

  

#### Windows

* Path to library **EaFort**: *C:\Users\user\Codes\EaFort*, 

```powershell
cmake -S. -Bbuild -DPATH_EaFort=C:/Users/user/Codes/EaFort
cmake --build build
```

**Verified:**

* OneAPI 2022 with intel fortran



