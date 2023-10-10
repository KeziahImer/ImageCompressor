# ImageCompressor: K-Means Image Compression in Haskell

ImageCompressor is a Haskell project that utilizes clustering (k-means algorithm) to compress an image. Instead of reading from a file, the input for the project should follow the specified grammar:

**Input Format:**

IN ::= POINT ' ' COLOR ('\n' POINT ' ' COLOR ) *
POINT ::= '(' int ',' int ')'
COLOR ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT ::= '0 '.. '255 '

For example:

(0,0) (33,18,109)
(0,1) (33,18,109)
(0,2) (33,21,109)
(0,3) (33,21,112)
(0,4) (33,25,112)
(0,5) (33,32,112)
(1,0) (33,18,109)
(1,1) (35,18,109)
(1,2) (35,21,109)
(1,3) (38,21,112)

**Output Format:**

OUT ::= CLUSTER *
CLUSTER ::= '- -\n' COLOR '\n -\n' ( POINT ' ' COLOR '\n ' ) *
POINT ::= '(' int ',' int ')'
COLOR ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT ::= '0 '.. '255 '

For example:

--
(77,63,204)
-
(0,1) (98,99,233)
(2,0) (88,77,211)
(0,2) (45,12,167)
--
(35,36,45)
-
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,1) (1,56,37)
(0,0) (66,20,26)
(2,2) (15,89,40)

## How to Run

To run the ImageCompressor, use the following command:

```sh
./imageCompressor -n N -l L -f F
```

- `-n N`: Number of colors in the final image.
- `-l L`: Convergence limit.
- `-f F`: Path to the file containing the colors of the pixels.

**Exemple d'Utilisation:**

```sh
./imageCompressor -n 2 -l 0.8 -f input.txt
```

This command will compress the image based on the specified parameters.

Feel free to experiment with different values of N and L to see how they affect the compressed image.
