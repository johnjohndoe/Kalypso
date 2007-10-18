package org.kalypso.risk.model.jaitests;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.AffineTransform;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class RotateImage{

   public static void  main(String[] args){
//      RotateImage roateImage = new RotateImage();
//      if(args.length <1) {
//         System.out.println("Enter a valid image file name");
//         System.exit(0);
//      }
//      Image img = readImage(args[0]);
      Image img = readImage("D:/__test/jai/test01.jpg");
      displayImage(img);
   }

   public static Image readImage(String imageName){
       Image image = Toolkit.getDefaultToolkit().getImage(imageName);
       MediaTracker imageTracker = new MediaTracker(new JPanel());
       imageTracker.addImage(image, 0);
       try{
        imageTracker.waitForID(0);
       }catch(InterruptedException e){ return null;}
       return image;
    }
    public static void displayImage(Image img){
       JFrame fr = new JFrame();
       fr.addWindowListener(
         new WindowAdapter() {
            public void windowClosing(WindowEvent e){
               System.exit(0);
            }
         } );

       ImagePanel pan = new ImagePanel(img);
    
       pan.setSize(256,256);
       fr.getContentPane().add(pan);
       fr.pack();
       fr.setSize(256,256);
       fr.show();
       pan.setRotationAngle(45.0);
   }
   static class ImagePanel extends JComponent {
      protected Image image;
      protected double rotAngle =0.0;
      AffineTransform atx = new AffineTransform();
      protected int imageWidth, imageHeight;

      public ImagePanel(){}
      public ImagePanel(Image img){ 
         image = img;
         imageWidth = img.getWidth(this);
         imageHeight = img.getHeight(this);
      }

      public void setImage(Image img){ 
         if(image == null) return;
         image = img; 
         imageWidth = img.getWidth(this);
         imageHeight = img.getHeight(this);
         repaint();
      }

      public void setRotationAngle(double degrees) {
         rotAngle = Math.toRadians(degrees);
         repaint();
      }

      public void paintComponent(Graphics g){
         Rectangle rect = this.getBounds();
         Graphics2D g2d = (Graphics2D)g;
         if(image != null) {
            atx.setToTranslation((rect.width-imageWidth)/2,(rect.height-imageHeight)/2);
            atx.rotate(rotAngle, imageWidth/2, imageHeight/2);
            g2d.drawImage(image, atx, this);
         }
      }
   }
}