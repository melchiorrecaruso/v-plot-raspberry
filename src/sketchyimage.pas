

unit sketchyimage;


interface

uses
  classes, graphics, fpimage, fpvectorial;

type
  tsketchyimage = class(tobject) 
    count:          longint;
    stype:          shortstring;
    data_in:        tbitmap;
    data_out:       tbitmap;
    scalefactor:    double;
    xcorrection:    double;
    ycorrection:    double;
    xoffset:        double;
    yoffset:        double;

    fbrightness_in:  longint;
    fbrightness_out: longint;
    fbrightness_avg: double;

    fheight:        longword;
    fwidth:         longword;
    fnibsize:       longint;
  public
    constructor create(const filename: rawbytestring);
    destructor destroy; override;
    function getdarkpixel: tpoint;

  published


    property brightness_in:  longint read fbrightness_in;
    property brightness_out: longint read fbrightness_out;
    property brightness_avg: double  read fbrightness_avg;



    property nibsize: longint  read fnibsize;

    property height:  longword read fheight;
    property width:   longword read fwidth;


  end;


implementation

type
  linedarknessmode = (linedarknessmodeavg, linedarknessmodeclear);

var
  gcounter: longint = 0;


constructor tsketchyimage.create(const filename: rawbytestring);
var
  i, j:  longint;
  color: tfpcolor;
  image: tportablenetworkgraphic;
begin
  inherited create;
  count           := 1;
  stype           := 'sketchyimage';
  scalefactor     := 1.0;
  xcorrection     := 0.0;
  ycorrection     := 0.0;
  xoffset         := 0.0;
  yoffset         := 0.0;

  fbrightness_in  := 0;
  fbrightness_out := 0;
  fbrightness_avg := 0.0;
  fnibsize        := 1;

  image := tportablenetworkgraphic.create;
  try
    image.loadfromfile(filename);
  except;

  end;
  fheight  := image.height;
  fwidth   := image.width;

  data_in  := tbitmap.create;
  data_in.setsize(width, height);

  data_out := tbitmap.create;
  data_out.setsize(width, height);

  for i := 0 to width - 1 do
    for j := 0 to height -1 do
    begin
      color := image.canvas.colors[i, j];
      data_in .canvas.colors[i, j] := color;
      inc(fbrightness_in, color.alpha);

      color.alpha := 255;
      data_out.canvas.colors[i, j] := color;
    end;
  fbrightness_out := fwidth*fheight*255;
  fbrightness_avg := fbrightness_in/(fwidth*fheight);

  freemem(image);
end;


function tsketchyimage.getdarkpixel: tpoint;
var
  i, j:  longword;
  alpha: longword;
  color: tfpcolor;
begin
  alpha    := 255;
  result.x := 0;
  result.y := 0;

  for i := 0 to fwidth -1 do
    for j := 0 to fheight -1 do
    begin
      color := data_in.canvas.colors[i, j];
      if color.alpha < alpha then
      begin
        alpha    := color.alpha;
        result.x := i;
        result.y := j;
      end;
    end;
end;

destructor tsketchyimage.destroy;
begin
  data_in.destroy;
  data_out.destroy;
  inherited destroy;
end;

//
// helper method called by :
// 1) sketchyimage_avgdarknessforline returns the average darkness for pixels under a line
// 2) sketchyimage_cleardarknessforline clears the darkness for pixels under the line, returns 0
//
float SketchyImage_darknessHelperForLine(SketchyImage *obj, float x1, float y1, float x2, float y2,LineDarknessMode mode){

    int kernelSize = obj->nibsize;
    if(kernelSize%2 == 0){
        kernelSize = kernelSize - 1;
    }

    int xpol = (x1-x2) < 0;
    int ypol = (y1-y2) < 0;
    if(xpol == 0){
        xpol = -1;
    }
    if(ypol == 0){
        ypol = -1;
    }

    float xd = fabs(x1-x2);
    float yd = fabs(y1-y2);
    float slope = (x1-x2)/(y1-y2);
    float totaldarkness = 0;
    float avg;
    if(xd > yd){
        int i;
        int cx = (int)x1;
        int cy = (int)y1;
        for(i=0;i<xd;i++){
            if(mode == lineDarknessModeAvg){
                int darkness = SketchyImage_kernelValueByXY(obj,(int)floor(cx),(int)floor(cy),false,kernelSize);
                totaldarkness = totaldarkness + darkness;
            }else{
                SketchyImage_kernelValueByXY(obj,cx,cy,true,kernelSize);
            }
            cx += xpol;
            cy = y1 + i/slope * xpol;
        }
        avg = totaldarkness/xd; 
    }else{
        int i;
        int cx = (int)x1;
        int cy = (int)y1;
        for(i=0;i<yd;i++){
            if(mode == lineDarknessModeAvg){
                int darkness = SketchyImage_kernelValueByXY(obj,cx,cy,false,kernelSize);
                totaldarkness = totaldarkness + darkness;
            }else{
                SketchyImage_kernelValueByXY(obj,cx,cy,true,kernelSize);
            }
            cy += ypol;
            cx = x1 + i*slope * ypol;
        }
        avg = totaldarkness/yd; 
    }
    return avg;
}

float SketchyImage_avgDarknessForLine(SketchyImage *obj, float x1, float y1, float x2, float y2){
    float avg = SketchyImage_darknessHelperForLine(obj,x1,y1,x2,y2,lineDarknessModeAvg);
    return avg;
}

void SketchyImage_clearDarknessForLine(SketchyImage *obj, float x1, float y1, float x2, float y2){
    SketchyImage_darknessHelperForLine(obj,x1,y1,x2,y2,lineDarknessModeClear);
}

Point *SketchyImage_bestPointOfNDestinationsFromXY2(SketchyImage *obj, int radius, int x, int y){

    x = x * obj->scaleFactor;
    y = y * obj->scaleFactor;

    float degree_to_radian_fact = 0.0174532925;
    int n = 360; //full circle scan
    int i;
    float best = 9999.0;
    int bestX = 0;
    int bestY = 0;
    gCounter ++;
    for(i=gCounter; i<n+gCounter; i+=3){

        int rx = cos(i*degree_to_radian_fact) * radius + x;
        int ry = sin(i*degree_to_radian_fact) * radius + y;

        if(rx < obj->width-1 && ry < obj->height-1 && rx > 0 && ry > 0){
            float avg = SketchyImage_avgDarknessForLine(obj,x,y,rx,ry);
            if(avg < best){
                best = avg;
                bestX = rx;
                bestY = ry;
                // if(avg < 100){
                //     break;
                // }
            }
        }

    }
    SketchyImage_clearDarknessForLine(obj,x,y,bestX,bestY);
    bestX = bestX/obj->scaleFactor;
    bestY = bestY/obj->scaleFactor;
    return Point_alloc((float)bestX,(float)bestY);

}

int pix(SketchyImage *obj,int pixelindex){
    if(pixelindex < (obj->width * obj->height)){
        return pixelindex;
    }
    return 0;
}

int SketchyImage_getPixel(SketchyImage *obj,int x, int y){
    if (y < 0 || y > obj->height-1 || x < 0 || x > obj->width-1){
        return 255;
    }
    int index = y * obj->width + x;
    return obj->imageData[pix(obj,index)];
}

int SketchyImage_kernelValueByXY(SketchyImage *obj,int x, int y, bool clear,int kernelSize){
    
    //the image data contains a 6 byte header
    //these are the threshold levels
    //this used to be fixed (36 spacing)
    //int levels[6] = {217, 180, 144, 108, 72, 36};
    if (x > obj->width-1 || y > obj->height-1 || x < 0 || y < 0){
        //out of bounds
        return -1;
    }
    int w = obj->width;
    int pixelindex = y*w + x;

    int pixelValue = obj->imageData[pixelindex];
    if(kernelSize > 1){
        int i;
        int limit = (kernelSize - 1) / 2.0;
        for(i=0;i<limit;i++){
            pixelValue += obj->imageData[pix(obj,pixelindex+1+i)];
            pixelValue += obj->imageData[pix(obj,pixelindex+obj->width+(i*obj->width))];
            pixelValue += obj->imageData[pix(obj,pixelindex-obj->width-(i*obj->width))];
            pixelValue += obj->imageData[pix(obj,pixelindex-1-i)];
        }
    }

    if(clear){
        int index;
        if(kernelSize > 1){
            int i;
            int limit = (kernelSize - 1) / 2.0;
            for(i=0;i<limit;i++){

                index = pix(obj,pixelindex+1+i);
                obj->brightness += (255-obj->imageData[index]);
                obj->imageData[index] = 255;
                obj->outputBrightness -= obj->outputImageData[index];
                obj->outputImageData[index] = 0;

                index = pix(obj,pixelindex+obj->width+(i*obj->width));
                obj->brightness += (255-obj->imageData[index]);
                obj->imageData[index] = 255;
                obj->outputBrightness -= obj->outputImageData[index];
                obj->outputImageData[index] = 0;

                index = pix(obj,pixelindex-obj->width-(i*obj->width));
                obj->brightness += (255-obj->imageData[index]);
                obj->imageData[index] = 255;
                obj->outputBrightness -= obj->outputImageData[index];
                obj->outputImageData[index] = 0;

                index = pix(obj,pixelindex-1-i);
                obj->brightness += (255-obj->imageData[index]);
                obj->imageData[index] = 255;
                obj->outputBrightness -= obj->outputImageData[index];
                obj->outputImageData[index] = 0;

            }
        }
        index = pixelindex;
        obj->brightness += (255-obj->imageData[index]);
        obj->imageData[index] = 255;
        obj->outputBrightness -= obj->outputImageData[index];
        obj->outputImageData[index] = 0;
    }

    return pixelValue;
}

void SketchyImage_saveStateAsPNG(SketchyImage *obj,const char *name){
    unsigned char *imd = obj->imageData;
    unsigned error = lodepng_encode_file(name, imd,obj->width, obj->height,LCT_GREY,8);
    if(error) printf("error %u: %s\n", error, lodepng_error_text(error));
}

void SketchyImage_saveAsPNG(SketchyImage *obj,const char *name){
    unsigned char *imdo = obj->outputImageData;
    unsigned erroro = lodepng_encode_file(name, imdo,obj->width, obj->height,LCT_GREY,8);
    if(erroro) printf("error %u: %s\n", erroro, lodepng_error_text(erroro)); 
}





#endif
