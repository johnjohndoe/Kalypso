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

import java.awt.GraphicsConfigTemplate;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.image.BufferedImage;

import javax.media.j3d.Appearance;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.Bounds;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.ExponentialFog;
import javax.media.j3d.GraphicsConfigTemplate3D;
import javax.media.j3d.Group;
import javax.media.j3d.Material;
import javax.media.j3d.PhysicalBody;
import javax.media.j3d.PhysicalEnvironment;
import javax.media.j3d.QuadArray;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Texture;
import javax.media.j3d.TextureAttributes;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.media.j3d.View;
import javax.media.j3d.ViewPlatform;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Vector3d;

import org.deegree.services.wts.RenderEngine;
import org.deegree.services.wts.ViewPoint;
import org.deegree.services.wts.WTSScene;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;


/**
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
abstract public class Abstract3DRenderEngine implements RenderEngine {
    protected WTSScene scene = null;
    protected float back_clipping = 0;
    protected float front_clipping = 0;

    /**
     * Creates a new Abstract3DRenderEngine object.
     *
     * @param scene 
     */
    public Abstract3DRenderEngine( WTSScene scene ) {
        this.scene = scene;
        // clipping default
        back_clipping = 15000f;
        front_clipping = 2f;
    }

    /**
     * Creates a new canvas each time this is called.
     *
     * The Canvas3D class provides a drawing canvas for 3D rendering.
     * The Canvas3D object extends the Canvas object to include 3D-related information such as the size of the
     * canvas in pixels, the Canvas3D's location, also in pixels, within a Screen3D object, and whether or not the
     * canvas has stereo enabled.
     * Because all Canvas3D objects contain a reference to a Screen3D object and because Screen3D objects define
     * the size of a pixel in physical units, Java 3D can convert a Canvas3D size in pixels to a physical world size
     * in meters. It can also determine the Canvas3D's position and orientation in the physical world.
     *
     * @return A new canvas instance
     */
    protected Canvas3D createCanvas( boolean offscreen ) {
        //This class is used to obtain a valid GraphicsConfiguration that can be used by Java 3D.
        //It instantiates objects and then sets all non-default attributes as desired.
        GraphicsDevice[] gd = GraphicsEnvironment.getLocalGraphicsEnvironment().getScreenDevices();
        GraphicsConfigTemplate3D gc3D = new GraphicsConfigTemplate3D();
        gc3D.setSceneAntialiasing( GraphicsConfigTemplate.PREFERRED );
        gc3D.setDoubleBuffer( GraphicsConfigTemplate3D.REQUIRED );

        Canvas3D canvas = new Canvas3D( gd[0].getBestConfiguration( gc3D ), offscreen );
      
        return canvas;
    }

    /** 
     * Build the scenegraph for the canvas
     */
    private TransformGroup createTransformGroup() {
        //creates the TransformGroup
        // The TransformGroup node specifies a single spatial transformation, via a Transform3D object,
        // that can position, orient, and scale all of its children.
        TransformGroup view_tg = new TransformGroup();

        //Specifies that the node allows access to its object's transform information.
        view_tg.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );

        //Specifies that the node allows writing its object's transform information.
        view_tg.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );

        //Specifies that this Node allows read access to its local coordinates to virtual world (Vworld) coordinates transform.
        view_tg.setCapability( TransformGroup.ALLOW_LOCAL_TO_VWORLD_READ );

        return view_tg;
    }

    /**
     * sets/defines the <tt>View</tt> of the scene and adds it to the submitted 
     * <tt>BranchGroup</tt>
     *
     * @param view the scenes view
     * @param view_group 
     */
    protected void setView( View view, BranchGroup view_group ) {
        TransformGroup view_tg = createTransformGroup();

        ViewPoint vp = scene.getViewPoint();

        //The ViewPatform class is used to set up the "view" side of a Java 3D scene graph.
        ViewPlatform camera = new ViewPlatform();

        //RELATIVE_TO_FIELD_OF_VIEW tells Java 3D that it should modify the eyepoint position so it is located 
        //  at the appropriate place relative to the window to match the specified field of view. 
        //This implies that the view frustum will change whenever the application changes the field of view. 
        camera.setViewAttachPolicy( View.RELATIVE_TO_FIELD_OF_VIEW );
        view.setFieldOfView( vp.getAoV() );

        view_tg.addChild( camera );

        Transform3D translation = new Transform3D();
        translation.setTranslation( 
                new Vector3d( -vp.getObserverPosition().x, vp.getObserverPosition().y, 
                              vp.getObserverPosition().z ) );

        //creates the POI (Point of View)
        Transform3D angle = new Transform3D();

        //rotY: Sets the value of this transform to a counter clockwise rotation about the y axis.
        angle.rotY( mapAngle( vp.getHDirection() ) );

        Transform3D angle2 = new Transform3D();

        //rotX:  Sets the value of this transform to a counter clockwise rotation about the x axis.
        angle2.rotX( vp.getVDirection() );
        angle.mul( angle2 );

        translation.mul( angle );

        view_tg.setTransform( translation );

        // The View object contains all parameters needed in rendering a three dimensional scene
        // from one viewpoint.
        //View view = u.getViewer().getView();
        view.setBackClipDistance( back_clipping );
        view.setFrontClipDistance( front_clipping );

        //creates the PhysicalBody and PhysicalEnvironment for the View
        //and attachs it to the View        
        view.setPhysicalEnvironment( new PhysicalEnvironment() );
        view.setPhysicalBody( new PhysicalBody() );

        view_group.addChild( view_tg );

        //attach the View to the ViewPlatform
        view.attachViewPlatform( camera );
    }

    /**
     *
     *
     * @param angle 
     *
     * @return 
     */
    private double mapAngle( double angle ) {
        angle = Math.toDegrees( angle );

        angle *= -1;
        angle += 180;

        while ( angle > 360 )
            angle -= 360;

        while ( angle < 0 )
            angle += 360;

        return Math.toRadians( angle );
    }

    /**
     * adds a background to the scene
     *
     * @param vp view point
     * @param world_group background
     */
    protected void addBackround( ViewPoint vp, Group world_group, String background ) {
        Point3d pp = vp.getObserverPosition();
        Point3d origin = new Point3d( -pp.x, pp.y, pp.z );
        ExponentialFog fog = new ExponentialFog();
        fog.setColor( new Color3f( 0.7f, 0.7f, 0.7f ) );
        fog.setDensity( 0.00001f );

        Bounds bounds = new BoundingSphere( origin, back_clipping * 1.1 );

        //bounds = new BoundingBox(new Point3d(-200,-10,-140), new Point3d(200,0.1,60) );
        fog.setInfluencingBounds( bounds );

        Group g = createBackground( vp, background );
        world_group.addChild( g );
        world_group.addChild( fog );
    }

    /**
     * creates the background of a scene depending of the view point definition
     *
     * @param vp view point
     *
     * @return <tt>BranchGroup</tt> representing the background
     */
    public BranchGroup createBackground( ViewPoint vp, String background ) {

        Point3d[] fp = vp.getFootprint();
        float[] vertices = {
            (float)-fp[1].x, (float)fp[1].y - 200, (float)fp[1].z, (float)-fp[0].x, 
            (float)fp[0].y - 200, (float)fp[0].z, (float)-fp[0].x, (float)fp[0].y + 5000, 
            (float)fp[0].z, (float)-fp[1].x, (float)fp[1].y + 5000, (float)fp[1].z
        };

        float[] tex_coords = { 0, 0, 1, 0, 1, 1, 0, 1 };

        QuadArray geom = new QuadArray( 4, QuadArray.COORDINATES | 
                                        QuadArray.TEXTURE_COORDINATE_2 );
        geom.setCoordinates( 0, vertices );
        geom.setTextureCoordinates( 0, 0, tex_coords );

        // load a texture image using the Java 3D texture loader
        WTSConfiguration conf = null;
        try {
            conf = WTSConfiguration_Impl.getInstance();
        } catch (Exception e) {
            System.out.println(e);
        }
        BufferedImage bkgr = conf.getBackground( background );        
        
        Texture tex = new com.sun.j3d.utils.image.TextureLoader( bkgr ).getTexture();

        Material material = new Material();

        Color3f white = new Color3f( 0.7f, 0.7f, 0.7f );

        //The Material object defines the appearance of an object under illumination. 
        //If the Material object in an Appearance object is null, lighting is disabled for all nodes 
        //  that use that Appearance object.         
        material.setAmbientColor( white );
        material.setDiffuseColor( white );
        material.setSpecularColor( white );
        material.setShininess( 0f );
        material.setLightingEnable( true );

        //The Appearance object defines all rendering state that can be set as a component object of a Shape3D node. 
        Appearance app = new Appearance();
        app.setTexture( tex );
        app.setMaterial( material );

        //ALLOW_TEXTURE_WRITE: Specifies that this Appearance object allows writing its texture component information.
        app.setCapability( Appearance.ALLOW_TEXTURE_WRITE );

        //TextureAttributes object defines attributes that apply to texture mapping.
        TextureAttributes texAttr = new TextureAttributes();

        //MODULATE: Modulate the object color with the texture color.
        texAttr.setTextureMode( TextureAttributes.MODULATE );
        app.setTextureAttributes( texAttr );

        // create a parent BranchGroup for the Background
        BranchGroup backgroundGroup = new BranchGroup();

        // assign the BranchGroup to the Background as geometry.
        //back.setGeometry( bgGeometry );        
        // add the Background node to its parent BranchGroup.
        backgroundGroup.addChild( new Shape3D( geom, app ) );
        return backgroundGroup;
    }

    /**
     * sets the scenes back clip distance. default is 15000
     *
     * @param distance 
     */
    public void setBackClipDistance( float distance ) {
        back_clipping = distance;
    }

    /**
     * sets the scenes front clip distance. default is 2
     *
     * @param distance 
     */
    public void setFrontClipDistance( float distance ) {
        front_clipping = distance;
    }
}