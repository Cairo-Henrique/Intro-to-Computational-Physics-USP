# Intro to Computational Physics - USP

![FORTRAN-77](https://img.shields.io/badge/FORTRAN--77-734F96?style=for-the-badge&logo=fortran&logoColor=white)
![GFortran](https://img.shields.io/badge/GFortran-734F96?style=for-the-badge&logo=fortran&logoColor=white)

Repository containing the projects developed by me in the course Introduction to Computational Physics (7600017), offered by the São Carlos Institute of Physics (IFSC-USP).

**Professor and author of the exercises:** José Abel Hoyos Neto

The course develops practical skills in scientific computing, numerical programming, and computational problem-solving through hands-on projects in FORTRAN-77. It covers algorithm development, numerical methods, mathematical modeling, stochastic simulations, data analysis, and the computational solution of differential equations.

The repository is organized into five computational projects and a complementary Jupyter notebook for scientific data visualization and analysis.

## Projects

- [Project 1 — Introduction to Programming](#project-1--introduction-to-programming)
- [Project 2 — Random Systems](#project-2--random-systems)
- [Project 3 — Numerical Calculus](#project-3--numerical-calculus)
- [Project 4 — Differential Equations](#project-4--differential-equations)
- [Project 5 — Kepler's Laws and the Three-Body Problem](#project-5--keplers-laws-and-the-three-body-problem)

### Project 1 — Introduction to programming

Introduction to scientific programming in FORTRAN-77 through a set of practical computational tasks. The project develops fundamental programming and algorithmic skills, including numerical calculations, vector and matrix operations, file input/output, sorting, iterative methods, numerical precision, permutation generation, determinant evaluation, and the solution of linear systems using Cramer's rule. It also introduces scientific computation and data visualization through the calculation and graphical analysis of geometric quantities in arbitrary dimensions.

### Project 2 — Random Systems

Computational study of stochastic systems using pseudorandom number generators and statistical methods. The project explores the generation and analysis of random sequences, Monte Carlo integration, statistical sampling, and the central limit theorem. It also investigates stochastic models of physical phenomena, including radioactive decay and random walks in one and two dimensions, examining probability distributions, mean-square displacement, diffusion, and entropy.

### Project 3 — Numerical Calculus

Implementation and analysis of numerical methods for differentiation, integration, and root-finding. The project explores finite-difference approximations of derivatives using different orders and stencils, numerical quadrature methods such as the trapezoidal, Simpson's, and Boole's rules, and iterative algorithms for solving nonlinear equations, including bisection, Newton-Raphson, and the secant method. Numerical accuracy, truncation errors, and convergence are investigated through systematic variations of the numerical step size.

### Project 4 — Differential Equations

Computational study of ordinary differential equations arising from Newtonian mechanics, using the Euler and Euler-Cromer numerical integration methods. The project investigates one-dimensional motion with air resistance, two-dimensional projectile trajectories under drag forces, and the dynamics of simple, damped, and periodically forced pendulums. It further explores nonlinear oscillations, phase-space trajectories, Poincaré sections, limit cycles, chaotic dynamics, and Lyapunov exponents, as well as dynamic stabilization in a vertically driven pendulum.

### Project 5 — Kepler's Laws and the Three-Body Problem

Computational investigation of gravitational dynamics and celestial mechanics through the numerical simulation of orbital systems. The project implements the Euler-Cromer and Verlet integration methods to study circular and elliptical orbits, analyze energy conservation and orbital stability, and numerically verify Kepler's three laws. It extends the analysis to the gravitational interaction between the Sun, Earth, and Jupiter, exploring orbital perturbations, planetary stability, and asteroid trajectories associated with Kirkwood gaps. The project also investigates three-body choreographies, including Lagrange's equilateral configuration and the periodic figure-eight orbit.

### Scientific Data Visualization — Jupyter Notebook

A collection of Python scripts for visualizing and analyzing the numerical results generated throughout the computational projects. Using NumPy and Matplotlib, the notebook processes data files produced by the FORTRAN-77 programs and generates graphical representations of statistical systems, numerical solutions of differential equations, and gravitational dynamics. It includes visualizations of random walks, probability distributions, particle trajectories, pendulum dynamics, phase-space diagrams, orbital motion, and three-body systems.

The notebook covers Projects 2, 4, and 5, providing a complementary environment for graphical analysis and interpretation of the numerical simulations.

## Repository Structure

```text
.
├── projeto-1/
├── projeto-2/
├── projeto-3/
├── projeto-4/
├── projeto-5/
├── projects-data-visualization.ipynb
└── README.md
```

## Compiling and Running on Linux

To compile the program using `gfortran`:

```bash
gfortran program.f -o program.exe
```

To run the compiled executable:

```bash
./program.exe
```
