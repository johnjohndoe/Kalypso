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
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.media.jai.codec.FileSeekableStream;



/**
 * 
 * <p>-----------------------------------------------------------------------</p>
 *
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class Block extends Group {
        
    private BufferedImage textureBi = null;
    
    public Block() {
        this( 0, 0, 1, 1, 0, 1, 0 );
    }
    
    public Block(float x, float y, float xsize, float ysize, float base, 
                  float height, float rotation) {
        addChild( addObject( x, y, xsize, ysize, base, height, rotation ) );
    }
    
    public Block(float x, float y, float xsize, float ysize, float base, 
                  float height, float rotation, String textureSource) {        
        textureBi = new BufferedImage( 128, 128, BufferedImage.TYPE_INT_RGB );                
        try {
            FileSeekableStream fss = new FileSeekableStream( textureSource );         
            RenderedOp ro = JAI.create( "stream", fss );
            textureBi = ro.getAsBufferedImage();
            fss.close();
        } catch(Exception e) {
            System.out.println(e);	
        }
        addChild( addObject( x, y, xsize, ysize, base, height, rotation ) );
    }
    
    public Block(float x, float y, float xsize, float ysize, float base, 
                  float height, float rotation, BufferedImage texture) {
        this.textureBi = texture;
        addChild( addObject( x, y, xsize, ysize, base, height, rotation ) );
    }
     
     
    private Appearance getBAppearance() {
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
        //material.setEmissiveColor( 0.4f, 0.4f, 0.4f );
                
        Texture texture = new TextureLoader( textureBi ).getTexture();
        
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
    
    public Group addObject(float x, float z, float xsize, float ysize, float base, 
                           float height, float rotation) {

        Color3f white = new Color3f(.9f, .9f, .9f);
        Color3f blue = new Color3f(0.4f, 0.7f, 1.0f);
        Color3f specular = new Color3f(0.7f, 0.7f, 0.7f);

        Material material = new Material();
        material.setAmbientColor(white);
        material.setDiffuseColor(blue);
        material.setSpecularColor(specular);
        material.setShininess(10f);        
        material.setLightingEnable(true);

        Appearance towerAp = getBAppearance();

        //TextureAttributes object defines attributes that apply to texture mapping.
        TextureAttributes texAttr = new TextureAttributes();
        //MODULATE: Modulate the object color with the texture color.
        texAttr.setTextureMode(TextureAttributes.MODULATE);
        towerAp.setTextureAttributes( texAttr );

        //object to be added - in this case a ColorCube defined by Java3D
        Box box = new Box( xsize,  height/2f, ysize, Box.GENERATE_NORMALS |
                           Box.GENERATE_TEXTURE_COORDS, towerAp );

        //A generalized transform object represented internally as a 4x4 double-precision floating point matrix.
        //A Transform3D is used to perform translations, rotations, and scaling and shear effects.
        Transform3D trans2 = new Transform3D();
        trans2.rotY( Math.toRadians(rotation));
        Transform3D trans = new Transform3D();              
        //positions the object
        trans.setTranslation(new Vector3d(x, base+height/2f, z));
        trans.mul( trans2 );
        TransformGroup tg = new TransformGroup();
        tg.addChild( box );
        //Specifies that the node allows access to its object's transform information.
        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        //Specifies that the node allows writing its object's transform information.
        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        tg.setTransform( trans );
        
        return tg;

    }
       
    
}
