package org.kalypso.risk.model.jaitests;

import java.awt.image.RenderedImage;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Vector;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;

import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.ImageEncoder;
import com.sun.media.jai.codec.TIFFEncodeParam;

public class Multipagetiff {

   public static PlanarImage readAsPlanarImage(String filename) {
      return JAI.create("fileload", filename);
   }

   public static void saveAsTIFF(RenderedImage image, String file )
       throws java.io.IOException{
      String filename = file;
      if(!filename.endsWith(".tiff"))filename = new String(file+".tiff");
      OutputStream out = new FileOutputStream(filename);
      TIFFEncodeParam param = new TIFFEncodeParam();
      ImageEncoder encoder = ImageCodec.createImageEncoder("TIFF", out, param);
      encoder.encode(image);
      out.close();
   }

   public static void saveAsMultipageTIFF(RenderedImage[] image, String file )
       throws java.io.IOException{
      String filename = file;
      if(!filename.endsWith(".tiff"))filename = new String(file+".tiff");
      OutputStream out = new FileOutputStream(filename);
      TIFFEncodeParam param = new TIFFEncodeParam();
      ImageEncoder encoder = ImageCodec.createImageEncoder("TIFF", out, param);
      Vector vector = new Vector();
      for(int i=1;i<image.length;i++) {
          vector.add(image[i]);
      }
      param.setExtraImages(vector.iterator());
      encoder.encode(image[0]);
      out.close();
   }

   public void createMultipageTiff(String[] filenames){
      RenderedImage image[] = new PlanarImage[filenames.length];
      for(int i=0;i<filenames.length;i++) {
          image[i] = readAsPlanarImage(filenames[i]);
      }
      try {
         saveAsMultipageTIFF(image, "multipagetiff");
      }
      catch (Exception e) {e.printStackTrace();}
   }

   public static void  main(String[] args){
      Multipagetiff mtiff = new Multipagetiff();
//      if(args.length <1) {
//         System.out.println("Enter a image file names");
//         System.exit(0);
//      }
      final String[] myArgs = new String[1];
      myArgs[0] = "D:/__test/jai/test01.jpg";
      mtiff.createMultipageTiff(myArgs);
   }
}
