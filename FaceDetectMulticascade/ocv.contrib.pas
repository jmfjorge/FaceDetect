(*
  **************************************************************************************************
  Project Delphi-OpenCV
  **************************************************************************************************
  Contributor:
  laentir Valetov
  email:laex@bk.ru
  **************************************************************************************************
  You may retrieve the latest version of this file at the GitHub,
  located at git://github.com/Laex/Delphi-OpenCV.git
  **************************************************************************************************
  License:
  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
  you may not use this file except in compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied. See the License for the specific language governing rights
  and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
  **************************************************************************************************
  Warning: Using Delphi XE3 syntax!
  **************************************************************************************************
  The Initial Developer of the Original Code:
  OpenCV: open source computer vision library
  Homepage:    http://opencv.org
  Online docs: http://docs.opencv.org
  Q&A forum:   http://answers.opencv.org
  Dev zone:    http://code.opencv.org
  **************************************************************************************************
  Original file:
  opencv\modules\contrib\include\opencv2\contrib.hpp
  *************************************************************************************************
*)

unit ocv.contrib;

{$I OpenCV.inc}

interface

uses
  ocv.core.types_c;

/// ****************************************************************************************\
// *                                   Adaptive Skin Detector                               *
// \****************************************************************************************/

Type
  TCvAdaptiveSkinDetector = class
  private const
    GSD_HUE_LT       = 3;
    GSD_HUE_UT       = 33;
    GSD_INTENSITY_LT = 15;
    GSD_INTENSITY_UT = 250;

  type
    THistogram = class
    private const
      HistogramSize = (GSD_HUE_UT - GSD_HUE_LT + 1);
    protected
      function findCoverageIndex(surfaceToCover: double; defaultValue: Integer = 0): Integer;
    public
      fHistogram: pCvHistogram;
      constructor create;
      destructor Destroy; override;
      procedure findCurveThresholds(Var x1, x2: Integer; percent: double = 0.05);
      procedure mergeWith(source: THistogram; weight: double);
    end;
  private
    nStartCounter, nFrameCount, nSkinHueLowerBound, nSkinHueUpperBound, nMorphingMethod, nSamplingDivider: Integer;
    fHistogramMergeFactor, fHuePercentCovered: double;
    histogramHueMotion, skinHueHistogram: THistogram;
    imgHueFrame, imgSaturationFrame, imgLastGrayFrame, imgMotionFrame, imgFilteredFrame: pIplImage;
    imgShrinked, imgTemp, imgGrayFrame, imgHSVFrame: pIplImage;

  protected
    procedure initData(src: pIplImage; widthDivider, heightDivider: Integer);
    procedure adaptiveFilter;
  public

    const
    MORPHING_METHOD_NONE         = 0;
    MORPHING_METHOD_ERODE        = 1;
    MORPHING_METHOD_ERODE_ERODE  = 2;
    MORPHING_METHOD_ERODE_DILATE = 3;

    constructor create(samplingDivider: Integer = 1; morphingMethod: Integer = MORPHING_METHOD_NONE);
    destructor Destroy; override;
    procedure process(inputBGRImage: pIplImage; outputHueMask: pIplImage); virtual;
  end;

procedure ASD_INTENSITY_SET_PIXEL(ptr: PByte; qq: uchar); inline;
{ (*pointer) = (unsigned char)qq; }

function ASD_IS_IN_MOTION(ptr: PByte; v, threshold: uchar): Boolean;
// ((abs((*(pointer)) - (v)) > (threshold)) ? true : false)

/// ****************************************************************************************\
// *                                  Fuzzy MeanShift Tracker                               *
// \****************************************************************************************/
//
// class CV_EXPORTS CvFuzzyPoint {
// public:
// double x, y, value;
//
// CvFuzzyPoint(double _x, double _y);
// };
//
// class CV_EXPORTS CvFuzzyCurve {
// private:
// std::vector<CvFuzzyPoint> points;
// double value, centre;
//
// bool between(double x, double x1, double x2);
//
// public:
// CvFuzzyCurve();
// ~CvFuzzyCurve();
//
// void setCentre(double _centre);
// double getCentre();
// void clear();
// void addPoint(double x, double y);
// double calcValue(double param);
// double getValue();
// void setValue(double _value);
// };
//
// class CV_EXPORTS CvFuzzyFunction {
// public:
// std::vector<CvFuzzyCurve> curves;
//
// CvFuzzyFunction();
// ~CvFuzzyFunction();
// void addCurve(CvFuzzyCurve *curve, double value = 0);
// void resetValues();
// double calcValue();
// CvFuzzyCurve *newCurve();
// };
//
// class CV_EXPORTS CvFuzzyRule {
// private:
// CvFuzzyCurve *fuzzyInput1, *fuzzyInput2;
// CvFuzzyCurve *fuzzyOutput;
// public:
// CvFuzzyRule();
// ~CvFuzzyRule();
// void setRule(CvFuzzyCurve *c1, CvFuzzyCurve *c2, CvFuzzyCurve *o1);
// double calcValue(double param1, double param2);
// CvFuzzyCurve *getOutputCurve();
// };
//
// class CV_EXPORTS CvFuzzyController {
// private:
// std::vector<CvFuzzyRule*> rules;
// public:
// CvFuzzyController();
// ~CvFuzzyController();
// void addRule(CvFuzzyCurve *c1, CvFuzzyCurve *c2, CvFuzzyCurve *o1);
// double calcOutput(double param1, double param2);
// };
//
// class CV_EXPORTS CvFuzzyMeanShiftTracker
// {
// private:
// class FuzzyResizer
// {
// private:
// CvFuzzyFunction iInput, iOutput;
// CvFuzzyController fuzzyController;
// public:
// FuzzyResizer();
// int calcOutput(double edgeDensity, double density);
// };
//
// class SearchWindow
// {
// public:
// FuzzyResizer *fuzzyResizer;
// int x, y;
// int width, height, maxWidth, maxHeight, ellipseHeight, ellipseWidth;
// int ldx, ldy, ldw, ldh, numShifts, numIters;
// int xGc, yGc;
// long m00, m01, m10, m11, m02, m20;
// double ellipseAngle;
// double density;
// unsigned int depthLow, depthHigh;
// int verticalEdgeLeft, verticalEdgeRight, horizontalEdgeTop, horizontalEdgeBottom;
//
// SearchWindow();
// ~SearchWindow();
// void setSize(int _x, int _y, int _width, int _height);
// void initDepthValues(IplImage *maskImage, IplImage *depthMap);
// bool shift();
// void extractInfo(IplImage *maskImage, IplImage *depthMap, bool initDepth);
// void getResizeAttribsEdgeDensityLinear(int &resizeDx, int &resizeDy, int &resizeDw, int &resizeDh);
// void getResizeAttribsInnerDensity(int &resizeDx, int &resizeDy, int &resizeDw, int &resizeDh);
// void getResizeAttribsEdgeDensityFuzzy(int &resizeDx, int &resizeDy, int &resizeDw, int &resizeDh);
// bool meanShift(IplImage *maskImage, IplImage *depthMap, int maxIteration, bool initDepth);
// };
//
// public:
// enum TrackingState
// {
// tsNone          = 0,
// tsSearching     = 1,
// tsTracking      = 2,
// tsSetWindow     = 3,
// tsDisabled      = 10
// };
//
// enum ResizeMethod {
// rmEdgeDensityLinear     = 0,
// rmEdgeDensityFuzzy      = 1,
// rmInnerDensity          = 2
// };
//
// enum {
// MinKernelMass           = 1000
// };
//
// SearchWindow kernel;
// int searchMode;
//
// private:
// enum
// {
// MaxMeanShiftIteration   = 5,
// MaxSetSizeIteration     = 5
// };
//
// void findOptimumSearchWindow(SearchWindow &searchWindow, IplImage *maskImage, IplImage *depthMap, int maxIteration, int resizeMethod, bool initDepth);
//
// public:
// CvFuzzyMeanShiftTracker();
// ~CvFuzzyMeanShiftTracker();
//
// void track(IplImage *maskImage, IplImage *depthMap, int resizeMethod, bool resetSearch, int minKernelMass = MinKernelMass);
// };
//
//
// namespace cv
// {
//
// class CV_EXPORTS Octree
// {
// public:
// struct Node
// {
// Node() {}
// int begin, end;
// float x_min, x_max, y_min, y_max, z_min, z_max;
// int maxLevels;
// bool isLeaf;
// int children[8];
// };
//
// Octree();
// Octree( const std::vector<Point3f>& points, int maxLevels = 10, int minPoints = 20 );
// virtual ~Octree();
//
// virtual void buildTree( const std::vector<Point3f>& points, int maxLevels = 10, int minPoints = 20 );
// virtual void getPointsWithinSphere( const Point3f& center, float radius,
// std::vector<Point3f>& points ) const;
// const std::vector<Node>& getNodes() const { return nodes; }
// private:
// int minPoints;
// std::vector<Point3f> points;
// std::vector<Node> nodes;
//
// virtual void buildNext(size_t node_ind);
// };
//
//
// class CV_EXPORTS Mesh3D
// {
// public:
// struct EmptyMeshException {};
//
// Mesh3D();
// Mesh3D(const std::vector<Point3f>& vtx);
// ~Mesh3D();
//
// void buildOctree();
// void clearOctree();
// float estimateResolution(float tryRatio = 0.1f);
// void computeNormals(float normalRadius, int minNeighbors = 20);
// void computeNormals(const std::vector<int>& subset, float normalRadius, int minNeighbors = 20);
//
// void writeAsVrml(const String& file, const std::vector<Scalar>& colors = std::vector<Scalar>()) const;
//
// std::vector<Point3f> vtx;
// std::vector<Point3f> normals;
// float resolution;
// Octree octree;
//
// const static Point3f allzero;
// };
//
// class CV_EXPORTS SpinImageModel
// {
// public:
//
// /* model parameters, leave unset for default or auto estimate */
// float normalRadius;
// int minNeighbors;
//
// float binSize;
// int imageWidth;
//
// float lambda;
// float gamma;
//
// float T_GeometriccConsistency;
// float T_GroupingCorespondances;
//
// /* public interface */
// SpinImageModel();
// explicit SpinImageModel(const Mesh3D& mesh);
// ~SpinImageModel();
//
// void setLogger(std::ostream* log);
// void selectRandomSubset(float ratio);
// void setSubset(const std::vector<int>& subset);
// void compute();
//
// void match(const SpinImageModel& scene, std::vector< std::vector<Vec2i> >& result);
//
// Mat packRandomScaledSpins(bool separateScale = false, size_t xCount = 10, size_t yCount = 10) const;
//
// size_t getSpinCount() const { return spinImages.rows; }
// Mat getSpinImage(size_t index) const { return spinImages.row((int)index); }
// const Point3f& getSpinVertex(size_t index) const { return mesh.vtx[subset[index]]; }
// const Point3f& getSpinNormal(size_t index) const { return mesh.normals[subset[index]]; }
//
// const Mesh3D& getMesh() const { return mesh; }
// Mesh3D& getMesh() { return mesh; }
//
// /* static utility functions */
// static bool spinCorrelation(const Mat& spin1, const Mat& spin2, float lambda, float& result);
//
// static Point2f calcSpinMapCoo(const Point3f& point, const Point3f& vertex, const Point3f& normal);
//
// static float geometricConsistency(const Point3f& pointScene1, const Point3f& normalScene1,
// const Point3f& pointModel1, const Point3f& normalModel1,
// const Point3f& pointScene2, const Point3f& normalScene2,
// const Point3f& pointModel2, const Point3f& normalModel2);
//
// static float groupingCreteria(const Point3f& pointScene1, const Point3f& normalScene1,
// const Point3f& pointModel1, const Point3f& normalModel1,
// const Point3f& pointScene2, const Point3f& normalScene2,
// const Point3f& pointModel2, const Point3f& normalModel2,
// float gamma);
// protected:
// void defaultParams();
//
// void matchSpinToModel(const Mat& spin, std::vector<int>& indeces,
// std::vector<float>& corrCoeffs, bool useExtremeOutliers = true) const;
//
// void repackSpinImages(const std::vector<uchar>& mask, Mat& spinImages, bool reAlloc = true) const;
//
// std::vector<int> subset;
// Mesh3D mesh;
// Mat spinImages;
// std::ostream* out;
// };
//
// class CV_EXPORTS TickMeter
// {
// public:
// TickMeter();
// void start();
// void stop();
//
// int64 getTimeTicks() const;
// double getTimeMicro() const;
// double getTimeMilli() const;
// double getTimeSec()   const;
// int64 getCounter() const;
//
// void reset();
// private:
// int64 counter;
// int64 sumTime;
// int64 startTime;
// };
//
// CV_EXPORTS std::ostream& operator<<(std::ostream& out, const TickMeter& tm);
//
// class CV_EXPORTS SelfSimDescriptor
// {
// public:
// SelfSimDescriptor();
// SelfSimDescriptor(int _ssize, int _lsize,
// int _startDistanceBucket=DEFAULT_START_DISTANCE_BUCKET,
// int _numberOfDistanceBuckets=DEFAULT_NUM_DISTANCE_BUCKETS,
// int _nangles=DEFAULT_NUM_ANGLES);
// SelfSimDescriptor(const SelfSimDescriptor& ss);
// virtual ~SelfSimDescriptor();
// SelfSimDescriptor& operator = (const SelfSimDescriptor& ss);
//
// size_t getDescriptorSize() const;
// Size getGridSize( Size imgsize, Size winStride ) const;
//
// virtual void compute(const Mat& img, std::vector<float>& descriptors, Size winStride=Size(),
// const std::vector<Point>& locations=std::vector<Point>()) const;
// virtual void computeLogPolarMapping(Mat& mappingMask) const;
// virtual void SSD(const Mat& img, Point pt, Mat& ssd) const;
//
// int smallSize;
// int largeSize;
// int startDistanceBucket;
// int numberOfDistanceBuckets;
// int numberOfAngles;
//
// enum { DEFAULT_SMALL_SIZE = 5, DEFAULT_LARGE_SIZE = 41,
// DEFAULT_NUM_ANGLES = 20, DEFAULT_START_DISTANCE_BUCKET = 3,
// DEFAULT_NUM_DISTANCE_BUCKETS = 7 };
// };
//
//
// typedef bool (*BundleAdjustCallback)(int iteration, double norm_error, void* user_data);
//
// class CV_EXPORTS LevMarqSparse {
// public:
// LevMarqSparse();
// LevMarqSparse(int npoints, // number of points
// int ncameras, // number of cameras
// int nPointParams, // number of params per one point  (3 in case of 3D points)
// int nCameraParams, // number of parameters per one camera
// int nErrParams, // number of parameters in measurement vector
// // for 1 point at one camera (2 in case of 2D projections)
// Mat& visibility, // visibility matrix. rows correspond to points, columns correspond to cameras
// // 1 - point is visible for the camera, 0 - invisible
// Mat& P0, // starting vector of parameters, first cameras then points
// Mat& X, // measurements, in order of visibility. non visible cases are skipped
// TermCriteria criteria, // termination criteria
//
// // callback for estimation of Jacobian matrices
// void (CV_CDECL * fjac)(int i, int j, Mat& point_params,
// Mat& cam_params, Mat& A, Mat& B, void* data),
// // callback for estimation of backprojection errors
// void (CV_CDECL * func)(int i, int j, Mat& point_params,
// Mat& cam_params, Mat& estim, void* data),
// void* data, // user-specific data passed to the callbacks
// BundleAdjustCallback cb, void* user_data
// );
//
// virtual ~LevMarqSparse();
//
// virtual void run( int npoints, // number of points
// int ncameras, // number of cameras
// int nPointParams, // number of params per one point  (3 in case of 3D points)
// int nCameraParams, // number of parameters per one camera
// int nErrParams, // number of parameters in measurement vector
// // for 1 point at one camera (2 in case of 2D projections)
// Mat& visibility, // visibility matrix. rows correspond to points, columns correspond to cameras
// // 1 - point is visible for the camera, 0 - invisible
// Mat& P0, // starting vector of parameters, first cameras then points
// Mat& X, // measurements, in order of visibility. non visible cases are skipped
// TermCriteria criteria, // termination criteria
//
// // callback for estimation of Jacobian matrices
// void (CV_CDECL * fjac)(int i, int j, Mat& point_params,
// Mat& cam_params, Mat& A, Mat& B, void* data),
// // callback for estimation of backprojection errors
// void (CV_CDECL * func)(int i, int j, Mat& point_params,
// Mat& cam_params, Mat& estim, void* data),
// void* data // user-specific data passed to the callbacks
// );
//
// virtual void clear();
//
// // useful function to do simple bundle adjustment tasks
// static void bundleAdjust(std::vector<Point3d>& points, // positions of points in global coordinate system (input and output)
// const std::vector<std::vector<Point2d> >& imagePoints, // projections of 3d points for every camera
// const std::vector<std::vector<int> >& visibility, // visibility of 3d points for every camera
// std::vector<Mat>& cameraMatrix, // intrinsic matrices of all cameras (input and output)
// std::vector<Mat>& R, // rotation matrices of all cameras (input and output)
// std::vector<Mat>& T, // translation vector of all cameras (input and output)
// std::vector<Mat>& distCoeffs, // distortion coefficients of all cameras (input and output)
// const TermCriteria& criteria=
// TermCriteria(TermCriteria::COUNT+TermCriteria::EPS, 30, DBL_EPSILON),
// BundleAdjustCallback cb = 0, void* user_data = 0);
//
// public:
// virtual void optimize(CvMat &_vis); //main function that runs minimization
//
// //iteratively asks for measurement for visible camera-point pairs
// void ask_for_proj(CvMat &_vis,bool once=false);
// //iteratively asks for Jacobians for every camera_point pair
// void ask_for_projac(CvMat &_vis);
//
// CvMat* err; //error X-hX
// double prevErrNorm, errNorm;
// double lambda;
// CvTermCriteria criteria;
// int iters;
//
// CvMat** U; //size of array is equal to number of cameras
// CvMat** V; //size of array is equal to number of points
// CvMat** inv_V_star; //inverse of V*
//
// CvMat** A;
// CvMat** B;
// CvMat** W;
//
// CvMat* X; //measurement
// CvMat* hX; //current measurement extimation given new parameter vector
//
// CvMat* prevP; //current already accepted parameter.
// CvMat* P; // parameters used to evaluate function with new params
// // this parameters may be rejected
//
// CvMat* deltaP; //computed increase of parameters (result of normal system solution )
//
// CvMat** ea; // sum_i  AijT * e_ij , used as right part of normal equation
// // length of array is j = number of cameras
// CvMat** eb; // sum_j  BijT * e_ij , used as right part of normal equation
// // length of array is i = number of points
//
// CvMat** Yj; //length of array is i = num_points
//
// CvMat* S; //big matrix of block Sjk  , each block has size num_cam_params x num_cam_params
//
// CvMat* JtJ_diag; //diagonal of JtJ,  used to backup diagonal elements before augmentation
//
// CvMat* Vis_index; // matrix which element is index of measurement for point i and camera j
//
// int num_cams;
// int num_points;
// int num_err_param;
// int num_cam_param;
// int num_point_param;
//
// //target function and jacobian pointers, which needs to be initialized
// void (*fjac)(int i, int j, Mat& point_params, Mat& cam_params, Mat& A, Mat& B, void* data);
// void (*func)(int i, int j, Mat& point_params, Mat& cam_params, Mat& estim, void* data);
//
// void* data;
//
// BundleAdjustCallback cb;
// void* user_data;
// };
//
// CV_EXPORTS_W int chamerMatching( Mat& img, Mat& templ,
// CV_OUT std::vector<std::vector<Point> >& results, CV_OUT std::vector<float>& cost,
// double templScale=1, int maxMatches = 20,
// double minMatchDistance = 1.0, int padX = 3,
// int padY = 3, int scales = 5, double minScale = 0.6, double maxScale = 1.6,
// double orientationWeight = 0.5, double truncate = 20);
//
//
// class CV_EXPORTS_W StereoVar
// {
// public:
// // Flags
// enum {USE_INITIAL_DISPARITY = 1, USE_EQUALIZE_HIST = 2, USE_SMART_ID = 4, USE_AUTO_PARAMS = 8, USE_MEDIAN_FILTERING = 16};
// enum {CYCLE_O, CYCLE_V};
// enum {PENALIZATION_TICHONOV, PENALIZATION_CHARBONNIER, PENALIZATION_PERONA_MALIK};
//
// //! the default constructor
// CV_WRAP StereoVar();
//
// //! the full constructor taking all the necessary algorithm parameters
// CV_WRAP StereoVar(int levels, double pyrScale, int nIt, int minDisp, int maxDisp, int poly_n, double poly_sigma, float fi, float lambda, int penalization, int cycle, int flags);
//
// //! the destructor
// virtual ~StereoVar();
//
// //! the stereo correspondence operator that computes disparity map for the specified rectified stereo pair
// CV_WRAP_AS(compute) virtual void operator()(const Mat& left, const Mat& right, CV_OUT Mat& disp);
//
// CV_PROP_RW int      levels;
// CV_PROP_RW double   pyrScale;
// CV_PROP_RW int      nIt;
// CV_PROP_RW int      minDisp;
// CV_PROP_RW int      maxDisp;
// CV_PROP_RW int      poly_n;
// CV_PROP_RW double   poly_sigma;
// CV_PROP_RW float    fi;
// CV_PROP_RW float    lambda;
// CV_PROP_RW int      penalization;
// CV_PROP_RW int      cycle;
// CV_PROP_RW int      flags;
//
// private:
// void autoParams();
// void FMG(Mat &I1, Mat &I2, Mat &I2x, Mat &u, int level);
// void VCycle_MyFAS(Mat &I1_h, Mat &I2_h, Mat &I2x_h, Mat &u_h, int level);
// void VariationalSolver(Mat &I1_h, Mat &I2_h, Mat &I2x_h, Mat &u_h, int level);
// };
//
// CV_EXPORTS void polyfit(const Mat& srcx, const Mat& srcy, Mat& dst, int order);
//
// class CV_EXPORTS Directory
// {
// public:
// static std::vector<String> GetListFiles  ( const String& path, const String & exten = "*", bool addPath = true );
// static std::vector<String> GetListFilesR ( const String& path, const String & exten = "*", bool addPath = true );
// static std::vector<String> GetListFolders( const String& path, const String & exten = "*", bool addPath = true );
// };
//
// /*
// * Generation of a set of different colors by the following way:
// * 1) generate more then need colors (in "factor" times) in RGB,
// * 2) convert them to Lab,
// * 3) choose the needed count of colors from the set that are more different from
// *    each other,
// * 4) convert the colors back to RGB
// */
// CV_EXPORTS void generateColors( std::vector<Scalar>& colors, size_t count, size_t factor=100 );
//
//
// /*
// *  Estimate the rigid body motion from frame0 to frame1. The method is based on the paper
// *  "Real-Time Visual Odometry from Dense RGB-D Images", F. Steinbucker, J. Strum, D. Cremers, ICCV, 2011.
// */
// enum { ROTATION          = 1,
// TRANSLATION       = 2,
// RIGID_BODY_MOTION = 4
// };
// CV_EXPORTS bool RGBDOdometry( Mat& Rt, const Mat& initRt,
// const Mat& image0, const Mat& depth0, const Mat& mask0,
// const Mat& image1, const Mat& depth1, const Mat& mask1,
// const Mat& cameraMatrix, float minDepth=0.f, float maxDepth=4.f, float maxDepthDiff=0.07f,
// const std::vector<int>& iterCounts=std::vector<int>(),
// const std::vector<float>& minGradientMagnitudes=std::vector<float>(),
// int transformType=RIGID_BODY_MOTION );
//
// /**
// *Bilinear interpolation technique.
// *
// *The value of a desired cortical pixel is obtained through a bilinear interpolation of the values
// *of the four nearest neighbouring Cartesian pixels to the center of the RF.
// *The same principle is applied to the inverse transformation.
// *
// *More details can be found in http://dx.doi.org/10.1007/978-3-642-23968-7_5
// */
// class CV_EXPORTS LogPolar_Interp
// {
// public:
//
// LogPolar_Interp() {}
//
// /**
// *Constructor
// *\param w the width of the input image
// *\param h the height of the input image
// *\param center the transformation center: where the output precision is maximal
// *\param R the number of rings of the cortical image (default value 70 pixel)
// *\param ro0 the radius of the blind spot (default value 3 pixel)
// *\param full \a 1 (default value) means that the retinal image (the inverse transform) is computed within the circumscribing circle.
// *            \a 0 means that the retinal image is computed within the inscribed circle.
// *\param S the number of sectors of the cortical image (default value 70 pixel).
// *         Its value is usually internally computed to obtain a pixel aspect ratio equals to 1.
// *\param sp \a 1 (default value) means that the parameter \a S is internally computed.
// *          \a 0 means that the parameter \a S is provided by the user.
// */
// LogPolar_Interp(int w, int h, Point2i center, int R=70, double ro0=3.0,
// int interp=INTER_LINEAR, int full=1, int S=117, int sp=1);
// /**
// *Transformation from Cartesian image to cortical (log-polar) image.
// *\param source the Cartesian image
// *\return the transformed image (cortical image)
// */
// const Mat to_cortical(const Mat &source);
// /**
// *Transformation from cortical image to retinal (inverse log-polar) image.
// *\param source the cortical image
// *\return the transformed image (retinal image)
// */
// const Mat to_cartesian(const Mat &source);
// /**
// *Destructor
// */
// ~LogPolar_Interp();
//
// protected:
//
// Mat Rsri;
// Mat Csri;
//
// int S, R, M, N;
// int top, bottom,left,right;
// double ro0, romax, a, q;
// int interp;
//
// Mat ETAyx;
// Mat CSIyx;
//
// void create_map(int M, int N, int R, int S, double ro0);
// };
//
// /**
// *Overlapping circular receptive fields technique
// *
// *The Cartesian plane is divided in two regions: the fovea and the periphery.
// *The fovea (oversampling) is handled by using the bilinear interpolation technique described above, whereas in
// *the periphery we use the overlapping Gaussian circular RFs.
// *
// *More details can be found in http://dx.doi.org/10.1007/978-3-642-23968-7_5
// */
// class CV_EXPORTS LogPolar_Overlapping
// {
// public:
// LogPolar_Overlapping() {}
//
// /**
// *Constructor
// *\param w the width of the input image
// *\param h the height of the input image
// *\param center the transformation center: where the output precision is maximal
// *\param R the number of rings of the cortical image (default value 70 pixel)
// *\param ro0 the radius of the blind spot (default value 3 pixel)
// *\param full \a 1 (default value) means that the retinal image (the inverse transform) is computed within the circumscribing circle.
// *            \a 0 means that the retinal image is computed within the inscribed circle.
// *\param S the number of sectors of the cortical image (default value 70 pixel).
// *         Its value is usually internally computed to obtain a pixel aspect ratio equals to 1.
// *\param sp \a 1 (default value) means that the parameter \a S is internally computed.
// *          \a 0 means that the parameter \a S is provided by the user.
// */
// LogPolar_Overlapping(int w, int h, Point2i center, int R=70,
// double ro0=3.0, int full=1, int S=117, int sp=1);
// /**
// *Transformation from Cartesian image to cortical (log-polar) image.
// *\param source the Cartesian image
// *\return the transformed image (cortical image)
// */
// const Mat to_cortical(const Mat &source);
// /**
// *Transformation from cortical image to retinal (inverse log-polar) image.
// *\param source the cortical image
// *\return the transformed image (retinal image)
// */
// const Mat to_cartesian(const Mat &source);
// /**
// *Destructor
// */
// ~LogPolar_Overlapping();
//
// protected:
//
// Mat Rsri;
// Mat Csri;
// std::vector<int> Rsr;
// std::vector<int> Csr;
// std::vector<double> Wsr;
//
// int S, R, M, N, ind1;
// int top, bottom,left,right;
// double ro0, romax, a, q;
//
// struct kernel
// {
// kernel() { w = 0; }
// std::vector<double> weights;
// int w;
// };
//
// Mat ETAyx;
// Mat CSIyx;
// std::vector<kernel> w_ker_2D;
//
// void create_map(int M, int N, int R, int S, double ro0);
// };
//
// /**
// * Adjacent receptive fields technique
// *
// *All the Cartesian pixels, whose coordinates in the cortical domain share the same integer part, are assigned to the same RF.
// *The precision of the boundaries of the RF can be improved by breaking each pixel into subpixels and assigning each of them to the correct RF.
// *This technique is implemented from: Traver, V., Pla, F.: Log-polar mapping template design: From task-level requirements
// *to geometry parameters. Image Vision Comput. 26(10) (2008) 1354-1370
// *
// *More details can be found in http://dx.doi.org/10.1007/978-3-642-23968-7_5
// */
// class CV_EXPORTS LogPolar_Adjacent
// {
// public:
// LogPolar_Adjacent() {}
//
// /**
// *Constructor
// *\param w the width of the input image
// *\param h the height of the input image
// *\param center the transformation center: where the output precision is maximal
// *\param R the number of rings of the cortical image (default value 70 pixel)
// *\param ro0 the radius of the blind spot (default value 3 pixel)
// *\param smin the size of the subpixel (default value 0.25 pixel)
// *\param full \a 1 (default value) means that the retinal image (the inverse transform) is computed within the circumscribing circle.
// *            \a 0 means that the retinal image is computed within the inscribed circle.
// *\param S the number of sectors of the cortical image (default value 70 pixel).
// *         Its value is usually internally computed to obtain a pixel aspect ratio equals to 1.
// *\param sp \a 1 (default value) means that the parameter \a S is internally computed.
// *          \a 0 means that the parameter \a S is provided by the user.
// */
// LogPolar_Adjacent(int w, int h, Point2i center, int R=70, double ro0=3.0, double smin=0.25, int full=1, int S=117, int sp=1);
// /**
// *Transformation from Cartesian image to cortical (log-polar) image.
// *\param source the Cartesian image
// *\return the transformed image (cortical image)
// */
// const Mat to_cortical(const Mat &source);
// /**
// *Transformation from cortical image to retinal (inverse log-polar) image.
// *\param source the cortical image
// *\return the transformed image (retinal image)
// */
// const Mat to_cartesian(const Mat &source);
// /**
// *Destructor
// */
// ~LogPolar_Adjacent();
//
// protected:
// struct pixel
// {
// pixel() { u = v = 0; a = 0.; }
// int u;
// int v;
// double a;
// };
// int S, R, M, N;
// int top, bottom,left,right;
// double ro0, romax, a, q;
// std::vector<std::vector<pixel> > L;
// std::vector<double> A;
//
// void subdivide_recursively(double x, double y, int i, int j, double length, double smin);
// bool get_uv(double x, double y, int&u, int&v);
// void create_map(int M, int N, int R, int S, double ro0, double smin);
// };
//
// CV_EXPORTS Mat subspaceProject(InputArray W, InputArray mean, InputArray src);
// CV_EXPORTS Mat subspaceReconstruct(InputArray W, InputArray mean, InputArray src);
//
// class CV_EXPORTS LDA
// {
// public:
// // Initializes a LDA with num_components (default 0) and specifies how
// // samples are aligned (default dataAsRow=true).
// LDA(int num_components = 0) :
// _num_components(num_components) {};
//
// // Initializes and performs a Discriminant Analysis with Fisher's
// // Optimization Criterion on given data in src and corresponding labels
// // in labels. If 0 (or less) number of components are given, they are
// // automatically determined for given data in computation.
// LDA(InputArrayOfArrays src, InputArray labels,
// int num_components = 0) :
// _num_components(num_components)
// {
// this->compute(src, labels); //! compute eigenvectors and eigenvalues
// }
//
// // Serializes this object to a given filename.
// void save(const String& filename) const;
//
// // Deserializes this object from a given filename.
// void load(const String& filename);
//
// // Serializes this object to a given cv::FileStorage.
// void save(FileStorage& fs) const;
//
// // Deserializes this object from a given cv::FileStorage.
// void load(const FileStorage& node);
//
// // Destructor.
// ~LDA() {}
//
// //! Compute the discriminants for data in src and labels.
// void compute(InputArrayOfArrays src, InputArray labels);
//
// // Projects samples into the LDA subspace.
// Mat project(InputArray src);
//
// // Reconstructs projections from the LDA subspace.
// Mat reconstruct(InputArray src);
//
// // Returns the eigenvectors of this LDA.
// Mat eigenvectors() const { return _eigenvectors; };
//
// // Returns the eigenvalues of this LDA.
// Mat eigenvalues() const { return _eigenvalues; }
//
// protected:
// bool _dataAsRow;
// int _num_components;
// Mat _eigenvectors;
// Mat _eigenvalues;
//
// void lda(InputArrayOfArrays src, InputArray labels);
// };
//
// class CV_EXPORTS_W FaceRecognizer : public Algorithm
// {
// public:
// //! virtual destructor
// virtual ~FaceRecognizer() {}
//
// // Trains a FaceRecognizer.
// CV_WRAP virtual void train(InputArrayOfArrays src, InputArray labels) = 0;
//
// // Updates a FaceRecognizer.
// CV_WRAP virtual void update(InputArrayOfArrays src, InputArray labels);
//
// // Gets a prediction from a FaceRecognizer.
// virtual int predict(InputArray src) const = 0;
//
// // Predicts the label and confidence for a given sample.
// CV_WRAP virtual void predict(InputArray src, CV_OUT int &label, CV_OUT double &confidence) const = 0;
//
// // Serializes this object to a given filename.
// CV_WRAP virtual void save(const String& filename) const;
//
// // Deserializes this object from a given filename.
// CV_WRAP virtual void load(const String& filename);
//
// // Serializes this object to a given cv::FileStorage.
// virtual void save(FileStorage& fs) const = 0;
//
// // Deserializes this object from a given cv::FileStorage.
// virtual void load(const FileStorage& fs) = 0;
//
// };
//
// CV_EXPORTS_W Ptr<FaceRecognizer> createEigenFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
// CV_EXPORTS_W Ptr<FaceRecognizer> createFisherFaceRecognizer(int num_components = 0, double threshold = DBL_MAX);
// CV_EXPORTS_W Ptr<FaceRecognizer> createLBPHFaceRecognizer(int radius=1, int neighbors=8,
// int grid_x=8, int grid_y=8, double threshold = DBL_MAX);
//
// enum
// {
// COLORMAP_AUTUMN = 0,
// COLORMAP_BONE = 1,
// COLORMAP_JET = 2,
// COLORMAP_WINTER = 3,
// COLORMAP_RAINBOW = 4,
// COLORMAP_OCEAN = 5,
// COLORMAP_SUMMER = 6,
// COLORMAP_SPRING = 7,
// COLORMAP_COOL = 8,
// COLORMAP_HSV = 9,
// COLORMAP_PINK = 10,
// COLORMAP_HOT = 11
// };
//
// CV_EXPORTS_W void applyColorMap(InputArray src, OutputArray dst, int colormap);
//
// CV_EXPORTS bool initModule_contrib();
// }

implementation

Uses
  ocv.core_c,
  ocv.imgproc_c,
  ocv.imgproc.types_c;

{ TCvAdaptiveSkinDetector.THistogram }

constructor TCvAdaptiveSkinDetector.THistogram.create;
Var
  _histogramSize: Integer;
  range: TSingleArray1D;
  ranges: TSingleArray2D;
begin
  _histogramSize := HistogramSize;
  range[0] := GSD_HUE_LT;
  range[1] := GSD_HUE_UT;
  ranges[0] := @range;
  fHistogram := cvCreateHist(1, @_histogramSize, CV_HIST_ARRAY, @ranges, 1);
  cvClearHist(fHistogram);
end;

destructor TCvAdaptiveSkinDetector.THistogram.Destroy;
begin
  cvReleaseHist(fHistogram);
  inherited;
end;

function TCvAdaptiveSkinDetector.THistogram.findCoverageIndex(surfaceToCover: double; defaultValue: Integer): Integer;
Var
  s: double;
  i: Integer;
begin
  s := 0;
  for i := 0 to HistogramSize - 1 do
  begin
    s := s + cvGetReal1D(fHistogram^.bins, i);
    if (s >= surfaceToCover) then
      Exit(i);
  end;
  Result := defaultValue;
end;

procedure TCvAdaptiveSkinDetector.THistogram.findCurveThresholds(var x1, x2: Integer; percent: double);
Var
  sum: double;
  i: Integer;
begin
  sum := 0;

  for i := 0 to HistogramSize - 1 do
    sum := sum + cvGetReal1D(fHistogram^.bins, i);

  x1 := findCoverageIndex(sum * percent, -1);
  x2 := findCoverageIndex(sum * (1 - percent), -1);

  if (x1 = -1) then
    x1 := GSD_HUE_LT
  else
    x1 := x1 + GSD_HUE_LT;

  if (x2 = -1) then
    x2 := GSD_HUE_UT
  else
    x2 := x2 + GSD_HUE_LT;
end;

procedure TCvAdaptiveSkinDetector.THistogram.mergeWith(source: THistogram; weight: double);
Var
  myweight, maxVal1, maxVal2, ff1, ff2: Single;
  f1, f2: PSingle;
  i: Integer;
begin
  myweight := 1 - weight;
  maxVal1 := 0;
  maxVal2 := 0;
  cvGetMinMaxHistValue(source.fHistogram, nil, @maxVal2);
  if (maxVal2 > 0) then
  begin
    cvGetMinMaxHistValue(fHistogram, nil, @maxVal1);
    if (maxVal1 <= 0) then
    begin
      for i := 0 to HistogramSize - 1 do
      begin
        f1 := cvPtr1D(fHistogram^.bins, i);
        f2 := cvPtr1D(source.fHistogram^.bins, i);
        f1^ := f2^;
      end;
    end
    else
    begin
      for i := 0 to HistogramSize - 1 do
      begin
        f1 := cvPtr1D(fHistogram^.bins, i);
        f2 := cvPtr1D(source.fHistogram^.bins, i);

        ff1 := (f1^ / maxVal1) * myweight;
        if (ff1 < 0) then
          ff1 := -ff1;

        ff2 := (f2^ / maxVal2) * weight;
        if (ff2 < 0) then
          ff2 := -ff2;
        f1^ := (ff1 + ff2);
      end;
    end;
  end;
end;

{ TCvAdaptiveSkinDetector }

procedure TCvAdaptiveSkinDetector.adaptiveFilter;
begin

end;

constructor TCvAdaptiveSkinDetector.create(samplingDivider, morphingMethod: Integer);
begin
  nSkinHueLowerBound := GSD_HUE_LT;
  nSkinHueUpperBound := GSD_HUE_UT;

  fHistogramMergeFactor := 0.05; // empirical result
  fHuePercentCovered := 0.95; // empirical result

  nMorphingMethod := morphingMethod;
  nSamplingDivider := samplingDivider;

  nFrameCount := 0;
  nStartCounter := 0;

  imgHueFrame := nil;
  imgMotionFrame := nil;
  imgTemp := nil;
  imgFilteredFrame := nil;
  imgShrinked := nil;
  imgGrayFrame := nil;
  imgLastGrayFrame := nil;
  imgSaturationFrame := nil;
  imgHSVFrame := nil;

  histogramHueMotion := THistogram.create;
  skinHueHistogram := THistogram.create;

end;

destructor TCvAdaptiveSkinDetector.Destroy;
begin
  cvReleaseImage(imgHueFrame);
  cvReleaseImage(imgSaturationFrame);
  cvReleaseImage(imgMotionFrame);
  cvReleaseImage(imgTemp);
  cvReleaseImage(imgFilteredFrame);
  cvReleaseImage(imgShrinked);
  cvReleaseImage(imgGrayFrame);
  cvReleaseImage(imgLastGrayFrame);
  cvReleaseImage(imgHSVFrame);
  histogramHueMotion.Free;
  skinHueHistogram.Free;
  inherited;
end;

procedure TCvAdaptiveSkinDetector.initData(src: pIplImage; widthDivider, heightDivider: Integer);
Var
  imageSize: TCvSize;
begin
  imageSize := cvSize(src^.width div widthDivider, src^.height div heightDivider);

  imgHueFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgShrinked := cvCreateImage(imageSize, IPL_DEPTH_8U, src^.nChannels);
  imgSaturationFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgMotionFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgTemp := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgFilteredFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgGrayFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgLastGrayFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 1);
  imgHSVFrame := cvCreateImage(imageSize, IPL_DEPTH_8U, 3);
end;

procedure TCvAdaptiveSkinDetector.process(inputBGRImage, outputHueMask: pIplImage);
Var
  src: pIplImage;
  h, v, i, l: Integer;
  isInit: Boolean;
//  pShrinked,
  pHueFrame,
  pMotionFrame,
  pLastGrayFrame,
  pFilteredFrame,
  pGrayFrame: PByte;
begin
  src := inputBGRImage;

  isInit := false;

  Inc(nFrameCount);

  if (imgHueFrame = nil) then
  begin
    isInit := true;
    initData(src, nSamplingDivider, nSamplingDivider);
  end;

//  pShrinked := imgShrinked^.imageData;
  pHueFrame := imgHueFrame^.imageData;
  pMotionFrame := imgMotionFrame^.imageData;
  pLastGrayFrame := imgLastGrayFrame^.imageData;
  pFilteredFrame := imgFilteredFrame^.imageData;
  pGrayFrame := imgGrayFrame^.imageData;

  if (src^.width <> imgHueFrame^.width) or (src^.height <> imgHueFrame^.height) then
  begin
    cvResize(src, imgShrinked);
    cvCvtColor(imgShrinked, imgHSVFrame, CV_BGR2HSV);
  end
  else
  begin
    cvCvtColor(src, imgHSVFrame, CV_BGR2HSV);
  end;

  cvSplit(imgHSVFrame, imgHueFrame, imgSaturationFrame, imgGrayFrame, nil);

  cvSetZero(imgMotionFrame);
  cvSetZero(imgFilteredFrame);

  l := imgHueFrame^.height * imgHueFrame^.width;

  for i := 0 to l - 1 do
  begin
    v := pGrayFrame^;
    if (v >= GSD_INTENSITY_LT) and (v <= GSD_INTENSITY_UT) then
    begin
      h := pHueFrame^;
      if (h >= GSD_HUE_LT) and (h <= GSD_HUE_UT) then
      begin
        if (h >= nSkinHueLowerBound) and (h <= nSkinHueUpperBound) then
          ASD_INTENSITY_SET_PIXEL(pFilteredFrame, h);

        if ASD_IS_IN_MOTION(pLastGrayFrame, v, 7) then
          ASD_INTENSITY_SET_PIXEL(pMotionFrame, h);
      end;
    end;
//    pShrinked := pShrinked + 3;
    Inc(pGrayFrame);
    Inc(pLastGrayFrame);
    Inc(pMotionFrame);
    Inc(pHueFrame);
    Inc(pFilteredFrame);
  end;

  if (isInit) then
    cvCalcHist(imgHueFrame, skinHueHistogram.fHistogram);

  cvCopy(imgGrayFrame, imgLastGrayFrame);

  cvErode(imgMotionFrame, imgTemp); // eliminate disperse pixels, which occur because of the camera noise
  cvDilate(imgTemp, imgMotionFrame);

  cvCalcHist(&imgMotionFrame, histogramHueMotion.fHistogram);

  skinHueHistogram.mergeWith(&histogramHueMotion, fHistogramMergeFactor);

  skinHueHistogram.findCurveThresholds(nSkinHueLowerBound, nSkinHueUpperBound, 1 - fHuePercentCovered);

  case nMorphingMethod of
    MORPHING_METHOD_ERODE:
      begin
        cvErode(imgFilteredFrame, imgTemp);
        cvCopy(imgTemp, imgFilteredFrame);
      end;
    MORPHING_METHOD_ERODE_ERODE:
      begin
        cvErode(imgFilteredFrame, imgTemp);
        cvErode(imgTemp, imgFilteredFrame);
      end;
    MORPHING_METHOD_ERODE_DILATE:
      begin
        cvErode(imgFilteredFrame, imgTemp);
        cvDilate(imgTemp, imgFilteredFrame);
      end;
  end;

  if (outputHueMask <> nil) then
    cvCopy(imgFilteredFrame, outputHueMask);
end;

procedure ASD_INTENSITY_SET_PIXEL(ptr: PByte; qq: uchar); inline;
begin
  // (*pointer) = (unsigned char)qq;
  ptr[0] := qq;
end;

function ASD_IS_IN_MOTION(ptr: PByte; v, threshold: uchar): Boolean;
begin
  // ((abs((*(pointer)) - (v)) > (threshold)) ? true : false)
  Result := Abs(ptr[0] - v) > threshold;
end;

end.
