package org.deegree_impl.services.wts;

import java.awt.image.BufferedImage;

import javax.media.j3d.Appearance;
import javax.media.j3d.Group;
import javax.media.j3d.Material;
import javax.media.j3d.Texture;
import javax.media.j3d.TextureAttributes;
import javax.media.j3d.Transform3D;
import javax.media.j3d.TransformGroup;
import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;
import javax.vecmath.Color3f;
import javax.vecmath.Vector3d;

import com.sun.j3d.utils.geometry.Box;
import com.sun.j3d.utils.geometry.Cylinder;
import com.sun.j3d.utils.geometry.Sphere;
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.media.jai.codec.FileSeekableStream;



/**
 * 
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class Tree extends Group {
	
	String textureSource = null;
    
    public Tree() {
        this( 0, 0, 1, 0, 1 );
    }
    
    public Tree(float x, float y, float size, float base,  float height ) {
        addChild( addObject( x, y, size, base, height ) );
    }
    
    public Tree(float x, float y, float size, float base, 
                  float height, String textureSource) {
        this.textureSource = textureSource;
        addChild( addObject( x, y, size, base, height ) );
    }
     
     
    private Appearance getBAppearance(String source) {
        Material material = new Material();

        Color3f white = new Color3f(0.7f, 0.7f, 0.7f );

        //The Material object defines the appearance of an object under illumination. 
        //If the Material object in an Appearance object is null, lighting is disabled for all nodes 
        //  that use that Appearance object.         
        material.setAmbientColor(white);
        material.setDiffuseColor(white);
        material.setSpecularColor(white);
        material.setShininess(1f);
        material.setLightingEnable(true);
        
        BufferedImage bi = new BufferedImage( 128, 128, BufferedImage.TYPE_INT_RGB );                
        try {
            FileSeekableStream fss = new FileSeekableStream( source );         
            RenderedOp ro = JAI.create( "stream", fss );
            bi = ro.getAsBufferedImage();
            fss.close();
        } catch(Exception e) {
            System.out.println(e);	
        }
        Texture texture = new TextureLoader( bi ).getTexture();
        
        //The Appearance object defines all rendering state that can be set as a component object of a Shape3D node. 
        Appearance app = new Appearance();
        app.setTexture( texture );
        app.setMaterial( material );        
        //ALLOW_TEXTURE_WRITE: Specifies that this Appearance object allows writing its texture component information.
        app.setCapability( Appearance.ALLOW_TEXTURE_WRITE );
        //TextureAttributes object defines attributes that apply to texture mapping.
        TextureAttributes texAttr = new TextureAttributes();
        //MODULATE: Modulate the object color with the texture color.
        texAttr.setTextureMode(TextureAttributes.MODULATE);
        app.setTextureAttributes(texAttr);      
        return app;
    }
    
    public Group addObject(float x, float z, float size, float base, float height) {

        Color3f white = new Color3f(.9f, .9f, .9f);
        Color3f blue = new Color3f(0.4f, 0.7f, 1.0f);
        Color3f specular = new Color3f(0.7f, 0.7f, 0.7f);

        Material material = new Material();
        material.setAmbientColor(white);
        material.setDiffuseColor(blue);
        material.setSpecularColor(specular);
        material.setShininess(10f);        
        material.setLightingEnable(true);

        Appearance crownAp = getBAppearance( "C:/java/source/j3dorg/crown.jpg" );

        //TextureAttributes object defines attributes that apply to texture mapping.
        TextureAttributes texAttr = new TextureAttributes( );
        //MODULATE: Modulate the object color with the texture color.
        texAttr.setTextureMode(TextureAttributes.MODULATE);
        crownAp.setTextureAttributes( texAttr );
        
        Appearance cylAp = getBAppearance( "C:/java/source/j3dorg/stamm.jpg" );

        //TextureAttributes object defines attributes that apply to texture mapping.
        texAttr = new TextureAttributes();
        //MODULATE: Modulate the object color with the texture color.
        texAttr.setTextureMode(TextureAttributes.MODULATE);
        cylAp.setTextureAttributes( texAttr );

        //object to be added - in this case a ColorCube defined by Java3D
        Sphere crown = new Sphere( size, Box.GENERATE_NORMALS |
                           Box.GENERATE_TEXTURE_COORDS, crownAp );
        Cylinder cyl = new Cylinder( size/7f, height, Cylinder.GENERATE_NORMALS |
                                     Cylinder.GENERATE_TEXTURE_COORDS, cylAp );
        
        Transform3D trans1 = new Transform3D();              
        //positions the object
        trans1.setTranslation(new Vector3d(0, height, 0));
        TransformGroup tg1 = new TransformGroup();
        tg1.addChild( crown );
        //Specifies that the node allows access to its object's transform information.
        tg1.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        //Specifies that the node allows writing its object's transform information.
        tg1.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        tg1.setTransform( trans1 );

        //A generalized transform object represented internally as a 4x4 double-precision floating point matrix.
        //A Transform3D is used to perform translations, rotations, and scaling and shear effects.        
        Transform3D trans = new Transform3D();              
        //positions the object
        trans.setTranslation(new Vector3d(x, base+height/2d, z));
        TransformGroup tg = new TransformGroup();
        tg.addChild( cyl );
        tg.addChild( tg1 );
        //Specifies that the node allows access to its object's transform information.
        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        //Specifies that the node allows writing its object's transform information.
        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        tg.setTransform( trans );
        
        return tg;

    }
    
}
