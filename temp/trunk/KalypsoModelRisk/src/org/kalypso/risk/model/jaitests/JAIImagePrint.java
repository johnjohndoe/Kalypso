package org.kalypso.risk.model.jaitests;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.image.RenderedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterJob;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

public class JAIImagePrint implements Printable {
   protected RenderedImage renderedImage;
   protected int imageWidth, imageHeight;
   protected Point printLoc = new Point(0,0);
   
  public static void  main(String[] args){
      JAIImagePrint sp = new JAIImagePrint();
//      if(args.length <1) 
//         System.out.println("Enter a valid image file name");
//      else sp.loadAndPrint(args[0]);
      sp.loadAndPrint("D:/__test/jai/test01.jpg");
      System.exit(0);
   }

   public void setPrintLocation(Point d) {
       printLoc = d;
   }

   public Point getPrintLocation() {
      return printLoc;
   }

   public void loadAndPrint(String filename){
      RenderedOp renderedOp = JAI.create("fileload", filename);
      renderedImage = renderedOp.createInstance();
      //Before printing, scale the image appropriately.
      imageWidth = renderedImage.getWidth();
      imageHeight = renderedImage.getHeight();
      //If the image is bigger than the page, you may to scale 
      //it to fit the paper.
      //Include your code here to scale the image.
 
      //Finally, print it
      print();
   }


   protected void print() {
      PrinterJob pj = PrinterJob.getPrinterJob();
      pj.setPrintable(this);
      //Include this statement if you need the print dialog
      //pj.printDialog();
      try{
          pj.print();
      }catch(Exception e){System.out.println(e);}
   }


   public int print(Graphics g, PageFormat f, int pageIndex){
       if(pageIndex >= 1) return Printable.NO_SUCH_PAGE;
       Graphics2D g2d = (Graphics2D) g;
       g2d.translate(f.getImageableX(), f.getImageableY());
       if(renderedImage != null){
          printImage(g2d, renderedImage);
          return Printable.PAGE_EXISTS;
       } else  return Printable.NO_SUCH_PAGE;
    }

    public void printImage(Graphics2D g2d, RenderedImage image){
       if((image == null)|| (g2d == null)) return;
       int x = printLoc.x;
       int y = printLoc.y;
       AffineTransform at = new AffineTransform();
       at.translate(x,y);
       g2d.drawRenderedImage(image,at);
   }
}
