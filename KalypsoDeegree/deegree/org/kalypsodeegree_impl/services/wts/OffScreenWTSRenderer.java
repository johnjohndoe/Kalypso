/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
Contact:
 
Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de
 
Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.services.wts;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;

import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.Group;
import javax.media.j3d.ImageComponent;
import javax.media.j3d.ImageComponent2D;
import javax.media.j3d.Light;
import javax.media.j3d.Locale;
import javax.media.j3d.OrderedGroup;
import javax.media.j3d.Shape3D;
import javax.media.j3d.View;
import javax.media.j3d.VirtualUniverse;

import org.deegree.services.wts.WTSScene;


/**
 * The class provides the capabilitiy for rendering a <tt>WTSScene</tt> to an
 * offscreen graphic context that is represent by a <tt>BufferedImage</tt>.
 * <p>-----------------------------------------------------------------------</p>
 *
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class OffScreenWTSRenderer extends Abstract3DRenderEngine {     
    
    private int width  = 800; 
    private int height = 600;    
    private Canvas3D offScreenCanvas3D = null;
    private View view = null;  
    private boolean newSize = false;
    /**
     * initialzies the render class with a default width and height (800x600)
     */
    public OffScreenWTSRenderer(WTSScene scene)
    {        
        this( scene, 801, 601 );          
    }
    
    /**
     * initialzies the render class with the submitted width and height 
     */
    public OffScreenWTSRenderer(WTSScene scene, int width, int height)
    {        
        super( scene );
        this.width = width;
        this.height = height;
        view = new View();        
    }
    
    public void setScene(WTSScene scene) {
        this.scene = scene;
    }
    
    /**Create the VirtualUniverse for the application.*/
    protected VirtualUniverse createVirtualUniverse() {
        return new VirtualUniverse();
    }
    
    /**
     * Simple utility method that creates a Locale for the
     * VirtualUniverse
     */
    protected Locale createLocale( VirtualUniverse u ) {
        return new Locale( u );
    }
    
    /**
     * returns the width of the offscreen rendering target
     */
    public int getWidth() {
        return width;
    }
    
    public void setWidth(int width_) {   
        newSize = true;
        this.width = width_;
    }
    
    /**
     * returns the height of the offscreen rendering target
     */
    public int getHeight() {
        return height;
    }
    
    public void setHeight(int height_) {
        newSize = true;
        this.height = height_;  
    }
    
    /** renders the scene to an <tt>BufferedImage</tt>
     * @return a <tt>BufferedImage</tt> where the scene has been rendered to
     */
    public Object renderScene() {
                
        if ( newSize ) {
            view.removeAllCanvas3Ds();
            offScreenCanvas3D = createOffscreenCanvas3D();          
            newSize = false;
        }
        view.addCanvas3D( offScreenCanvas3D );     
        
        view.startView();
        
        // The BranchGroup serves as a pointer to the root of a scene graph branch; 
        // BranchGroup objects are the only objects that can be inserted into a 
        // Locale's set of objects.         
        BranchGroup view_group = new BranchGroup();         
        BranchGroup world_object_group = new BranchGroup();        
        
        setView( view,  view_group );            

        OrderedGroup terrain_group = new OrderedGroup(); 
        addBackround( scene.getViewPoint(), terrain_group, "" + scene.getBackground());
        
        // add the lights to the view
        Light[] lights = scene.getLights();
        for (int i = 0; i < lights.length; i++) {
            view_group.addChild( lights[i] );
        }
                
        // add the terrain to the view
        Shape3D terrain[] = scene.getTerrain();
        for (int i = terrain.length-1; i >= 0; i--) {
            terrain_group.addChild( terrain[i] );
        }        
        world_object_group.addChild( terrain_group );
        
        // add the features to the view
        Group[] features = scene.getFeatures();
        for (int i = 0; i < features.length; i++) {                        
            world_object_group.addChild( features[i] );
        }                
        
        world_object_group.compile();
        view_group.compile();
        
        // A Locale object defines a high-resolution position within a 
        // VirtualUniverse, and serves as a container for a collection of 
        // BranchGroup-rooted subgraphs (branch graphs), at that position.
        VirtualUniverse universe = createVirtualUniverse();
        Locale locale = createLocale( universe );  
        // Adds them to the locale        
        locale.addBranchGraph(world_object_group);
        locale.addBranchGraph(view_group);
        
        RenderedImage image = getImage( offScreenCanvas3D );               
        
        universe.removeAllLocales();
        view.removeAllCanvas3Ds();
        view.stopView();
        
        return image;
    }

    
    /**
     * creates and returns a canvas for offscreen rendering
     */
    protected Canvas3D createOffscreenCanvas3D()
    {
        //First we create a Canvas3D and specify that it is to be used
        //for offscreen rendering.
        Canvas3D offScreenCanvas3D = createCanvas( true );
        //We then need to explicitly set the size of the off screen
        //Canvas3D.
        offScreenCanvas3D.getScreen3D().setSize( width, height );
        //This calculation returns the physical size of the screen and
        //is based on 90 display pixels per inch
        offScreenCanvas3D.getScreen3D().setPhysicalScreenHeight( 0.0254/90 * height );
        offScreenCanvas3D.getScreen3D().setPhysicalScreenWidth( 0.0254/90 * width );
        //We then create an AWT RenderedImage that the Canvas3D will
        //render into. We create a simple 3 Byte RGB format image.
        BufferedImage renderedImage = 
            new BufferedImage( width, height, BufferedImage.TYPE_3BYTE_BGR );        
        //The AWT RenderedImage needs to be wrapped in a Java 3D
        //ImageComponent2D before it can be assigned to the
        //Canvas3D for rendering
        ImageComponent2D imageComponent = 
            new ImageComponent2D( ImageComponent.FORMAT_RGB8, renderedImage );
        //This call notifies Java 3D that we require read-access to the
        //ImageComponent2D. We will be reading the pixels in the image
        //when we output it to disk.
        imageComponent.setCapability( ImageComponent2D.ALLOW_IMAGE_READ );
        //Finally, we assign the ImageComponent2D to the offscreen
        //Canvas3D for rendering
        offScreenCanvas3D.setOffScreenBuffer( imageComponent );
        return offScreenCanvas3D;
    }
    
    /*
     * Called to render the scene into the offscreen Canvas3D
     */
    protected RenderedImage getImage(Canvas3D offScreenCanvas3D)
    {        
        offScreenCanvas3D.renderOffScreenBuffer();
        offScreenCanvas3D.waitForOffScreenRendering();
        ImageComponent2D imageComponent = offScreenCanvas3D.getOffScreenBuffer();
        return imageComponent.getImage();
            
    }
}
