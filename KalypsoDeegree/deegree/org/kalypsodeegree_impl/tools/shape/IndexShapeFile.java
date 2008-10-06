/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/

package org.kalypsodeegree_impl.tools.shape;

import javax.swing.JFileChooser;
import javax.swing.JFrame;

/**
 * <p>
 * IndexShapeFile is an application that can be used to index an ESRI ShapeFiles(tm). It indexes both the geometry and
 * the alphanumeric attributes
 * </p>
 * 
 * <p>
 * The application shows a file chooser with which the user can select a file. When a file is choosen the application
 * opens it and shows the attributes. The user can now select the attributes that has to be indexed. Already indexed
 * attributes are already selected and can be deselected. For alphanumeric attributes the user can indicate if the
 * attribute is unique or not.
 * </p>
 * 
 * <p>
 * After selecting the attributes the application creates the needed indexes and loops over all the features in the
 * shape file. For every feature the application inserts the attributes in the right index. After looping over the
 * features the application closes the shapefile and the created indexes and removes the indexes that are no longer
 * needed (eg. the index for the attributes that are deselected).
 * </p>
 * 
 * <p>
 * It is not possible to transform a unique index to a non-unique index or back.
 * </p>
 */
public class IndexShapeFile
{
  public static void main( String[] args ) throws Exception
  {
    JFileChooser fileChooser = new JFileChooser( "C:/temp/wupper" );
    fileChooser.setFileFilter( new ShapeFilter() );
    fileChooser.setFileView( new ShapeView() );
    if( fileChooser.showOpenDialog( null ) == JFileChooser.APPROVE_OPTION )
    {
      IndexFrame indexFrame = new IndexFrame( fileChooser.getSelectedFile() );
      indexFrame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
      indexFrame.show();
    }
    else
      System.exit( 0 );
  }
}