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
package org.deegree.graphics.sld;

import java.awt.image.BufferedImage;

import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;

/**
 * A Mark takes a "shape" and applies coloring to it. The shape can be derived
 * either from a well-known name (such as "square"), an external URL in various
 * formats (such as, perhaps GIF), or from a glyph of a font. Multiple external
 * formats may be used with the semantic that they all contain the equivalent
 * shape in different formats. If an image format is used that has inherent
 * coloring, the coloring is discarded and only the opacity channel (or
 * equivalent) is used. A Halo, Fill, and/or Stroke is applied as appropriate
 * for the shape's source format.
 * <p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public interface Mark {

   /**
    * Gives the well known name of a Mark's shape. Allowed values include at
    * least "square", "circle", "triangle", "star", "cross", and "x", though map
    * servers may draw a different symbol instead if they don't have a shape for
    * all of these. Renderings of these marks may be made solid or hollow
    * depending on Fill and Stroke parameters. The default value is "square".
    * @return the WK-Name of the mark
    */
    String getWellKnownName();

   /**
    * Sets the well known name of a Mark's shape. Allowed values include at
    * least "square", "circle", "triangle", "star", "cross", and "x", though map
    * servers may draw a different symbol instead if they don't have a shape for
    * all of these. Renderings of these marks may be made solid or hollow
    * depending on Fill and Stroke parameters. The default value is "square"..
    * @param wellKnownName the WK-Name of the mark
    */
    void setWellKnownName(String wellKnownName);    
    
   /**
    * A Fill allows area geometries to be filled. There are two types of fills:
    * solid-color and repeated GraphicFill. In general, if a Fill element is
    * omitted in its containing element, no fill will be rendered. The default
    * is a solid 50%-gray (color "#808080") opaque fill.
    * @return the fill of the mark
    */
    Fill getFill();

   /**
    * Sets the Fill
    * @param fill the fill of the mark
    */
    void setFill(Fill fill);    
    
    /**
    * A Stroke allows a string of line segments (or any linear geometry) to be
    * rendered. There are three basic types of strokes: solid Color, GraphicFill
    * (stipple), and repeated GraphicStroke. A repeated graphic is plotted
    * linearly and has its graphic symbol bended around the curves of the line
    * string. The default is a solid black line (Color "#000000").
    * @return the stroke of the mark
    */
    Stroke getStroke();

   /**
    * Sets the <Stroke>
    * @param stroke the stroke of the mark
    */
    void setStroke(Stroke stroke);    
    
   /**
    * Returns the mark as an image. Rhis method is not part of the sld
    * specifications but it is added to speed up applications.
    * @return the mark as image
    */     
    BufferedImage getAsImage (Feature feature, int size)
        throws FilterEvaluationException;
    
   /**
    * Sets the mark as an image. Rhis method is not part of the sld
    * specifications but it is added to speed up applications.
    * @param bufferedImage the bufferedImage to be set for the mark
    */     
    void setAsImage (BufferedImage bufferedImage);   
}
