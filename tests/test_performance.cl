// Test source code is from http://musicdsp.org/archive.php?classid=0#1 and 
// modified to pass clang C compiler and changed TF to be local var in main
// to support ClampPointers pass.

// FormantsAM.cpp

// Thierry Rochebois' "Formantic Synthesis by Double Amplitude Modulation"

// Based on a tutorial by Thierry Rochebois.
// Comments by Paul Sernine.

// The spectral content of the signal is obtained by adding amplitude modulated formantic
// waveforms. The amplitude modulations spectraly shift the formantic waveforms.
// Continuous spectral shift, without losing the harmonic structure, is obtained
// by using crossfaded double carriers (multiple of the base frequency).
// To avoid  unwanted interference artifacts, phase relationships must be of the
// "cosine type".

// The output is a 44100Hz 16bit stereo PCM file.

// NOTE: for now looks like -O3 cripples safe exception load/store analysis
// TODO: get times from unclamped versions and compare to clamped ones and expect perf hit to be max 30%

// RUN: clang -target spir -S -c $TEST_SRC -O0 -emit-llvm -S -o $OUT_FILE.O0.ll &&
// RUN: echo "Running and verifying 'Formantic Synthesis by Double Amplitude Modulation' case" &&
// RUN: opt -S -O3 $OUT_FILE.O0.ll -o $OUT_FILE.O3.ll &&
// RUN: opt -S -load $CLAMP_PLUGIN -clamp-pointers $OUT_FILE.O0.ll -o $OUT_FILE.O0.clamped.ll &&
// RUN: opt -S -load $CLAMP_PLUGIN -clamp-pointers $OUT_FILE.O3.ll -o $OUT_FILE.O3.clamped.ll &&
// RUN: opt -S -O3 $OUT_FILE.O0.clamped.ll -o $OUT_FILE.O0.clamped.O3.ll &&
// RUN: opt -S -O3 $OUT_FILE.O3.clamped.ll -o $OUT_FILE.O3.clamped.O3.ll &&
// RUN: echo "Running original.O0:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O0.ll test_kernel 1 "(int,{0}):(int,1)") && 
// RUN: echo "Running original.O3:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O3.ll test_kernel 1 "(int,{0}):(int,1)") && 
// RUN: echo "Running O0.clamped:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O0.clamped.ll test_kernel 1 "(int,{0}):(int,1)") &&
// RUN: echo "Running O0.clamped.O3:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O0.clamped.O3.ll test_kernel 1 "(int,{0}):(int,1)") &&
// RUN: echo "Running O3.clamped:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O3.clamped.ll test_kernel 1 "(int,{0}):(int,1)") &&
// RUN: echo "Running O3.clamped.O3:" &&
// RUN: (BENCHMARK=1 $RUN_KERNEL $OUT_FILE.O3.clamped.O3.ll test_kernel 1 "(int,{0}):(int,1)")

typedef int int32_t;
typedef short int16_t;

float expf(float f);
float sinf(float f);
float cosf(float f);
float floorf(float f);
float powf(float a, float b);
float fmodf(float x, float y);

//Approximates cos(pi*x) for x in [-1,1].
float fast_cos(const float x)
{
  float x2 = x*x;
  return 1+x2*(-4+2*x2);
}

// Length of the table
#define L_TABLE (256+1) //The last entry of the table equals the first (to avoid a modulo)
// Maximal formant width
#define I_MAX 64

//Formantic function of width I (used to fill the table of formants)
float fonc_formant(float p,const float I)
{
  float a=0.5f;
  int32_t hmax=(int)(10*I)>L_TABLE/2?L_TABLE/2:(int32_t)(10*I);
  float phi=0.0f;
  for(int32_t h=1;h<hmax;h++)
    {
      phi+=3.14159265359f*p;
      float hann=0.5f+0.5f*fast_cos(h*(1.0f/hmax));
      float gaussienne=0.85f*expf(-h*h/(I*I));
      float jupe=0.15f;
      float harmonique=cosf(phi);
      a+=hann*(gaussienne+jupe)*harmonique;
    }
  return a;
}

//Initialisation of the table TF with the fonction fonc_formant.
void init_formant(float TF[])
{ float coef=2.0f/(L_TABLE-1);
  for(int32_t I=0;I<I_MAX;I++)
    for(int32_t P=0;P<L_TABLE;P++)
      TF[P+I*L_TABLE]=fonc_formant(-1+P*coef,(float)(I));
}

//This function emulates the function fonc_formant
// thanks to the table TF. A bilinear interpolation is
// performed
float formant(float p,float i, float TF[])
{
  i=i<0?0:i>I_MAX-2?I_MAX-2:i;    // width limitation
  float P=(L_TABLE-1)*(p+1)*0.5f; // phase normalisation
  int32_t P0=(int32_t)P;     float fP=P-P0;  // Integer and fractional
  int32_t I0=(int32_t)i;     float fI=i-I0;  // parts of the phase (p) and width (i).
  int32_t i00=P0+L_TABLE*I0;     int32_t i10=i00+L_TABLE;
  //bilinear interpolation.
  return (1-fI)*(TF[i00] + fP*(TF[i00+1]-TF[i00]))
    +    fI*(TF[i10] + fP*(TF[i10+1]-TF[i10]));
}

// Double carrier.
// h : position (float harmonic number)
// p : phase
float porteuse(const float h,const float p)
{
  float h0=floorf(h);  //integer and
  float hf=h-h0;      //decimal part of harmonic number.
  // modulos pour ramener p*h0 et p*(h0+1) dans [-1,1]
  float phi0=fmodf(p* h0   +1+1000,2.0f)-1.0f;
  float phi1=fmodf(p*(h0+1)+1+1000,2.0f)-1.0f;
  // two carriers.
  float Porteuse0=fast_cos(phi0);  float Porteuse1=fast_cos(phi1);
  // crossfade between the two carriers.
  return Porteuse0+hf*(Porteuse1-Porteuse0);
}

__kernel void test_kernel(__global int* out_val)
{
  //Formant table for various french vowels (you can add your own)
  float F1[]={  730,  200,  400,  250,  190,  350,  550,  550,  450};
  float A1[]={ 1.0f, 0.5f, 1.0f, 1.0f, 0.7f, 1.0f, 1.0f, 0.3f, 1.0f};
  float F2[]={ 1090, 2100,  900, 1700,  800, 1900, 1600,  850, 1100};
  float A2[]={ 2.0f, 0.5f, 0.7f, 0.7f,0.35f, 0.3f, 0.5f, 1.0f, 0.7f};
  float F3[]={ 2440, 3100, 2300, 2100, 2000, 2500, 2250, 1900, 1500};
  float A3[]={ 0.3f,0.15f, 0.2f, 0.4f, 0.1f, 0.3f, 0.7f, 0.2f, 0.2f};
  float F4[]={ 3400, 4700, 3000, 3300, 3400, 3700, 3200, 3000, 3000};
  float A4[]={ 0.2f, 0.1f, 0.2f, 0.3f, 0.1f, 0.1f, 0.3f, 0.2f, 0.3f};

  float f0,dp0,p0=0.0f;
  int32_t F=7; //number of the current formant preset
  float f1,f2,f3,f4,a1,a2,a3,a4;
  f1=f2=f3=f4=100.0f;a1=a2=a3=a4=0.0f;

  //Table of formants
  float TF[L_TABLE*I_MAX];

  init_formant(TF);

  for(int ns=0;ns<200*44100;ns++)
    {
      if(0==(ns%11025)){F++;F%=8;} //formant change
      f0=12*powf(2.0f,4-4*ns/(10*44100.0f)); //sweep
      f0*=(1.0f+0.01f*sinf(ns*0.0015f));        //vibrato
      dp0=f0*(1/22050.0f);
      float un_f0=1.0f/f0;
      p0+=dp0;  //phase increment
      p0-=2*(p0>1);
      { //smoothing of the commands.
        float r=0.001f;
        f1+=r*(F1[F]-f1);f2+=r*(F2[F]-f2);f3+=r*(F3[F]-f3);f4+=r*(F4[F]-f4);
        a1+=r*(A1[F]-a1);a2+=r*(A2[F]-a2);a3+=r*(A3[F]-a3);a4+=r*(A4[F]-a4);
      }

      //The f0/fn coefficients stand for a -3dB/oct spectral enveloppe
      float out=
        a1*(f0/f1)*formant(p0,100*un_f0,TF)*porteuse(f1*un_f0,p0)
        +0.7f*a2*(f0/f2)*formant(p0,120*un_f0,TF)*porteuse(f2*un_f0,p0)
        +     a3*(f0/f3)*formant(p0,150*un_f0,TF)*porteuse(f3*un_f0,p0)
        +     a4*(f0/f4)*formant(p0,300*un_f0,TF)*porteuse(f4*un_f0,p0);

      // write to volatile variable to test... (use puts to verify correctness)
      out_val[0] = (int16_t)(15000.0f*out);
    }
}
