/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

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
 katharina.lupp@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/*
 * Created on 20.01.2005
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Position;

import com.braju.format.Format;

/**
 * ----------------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class LineBC
{

  /**
   * Constructor
   */
  public LineBC()
  {

  }

  /**
   * creates continuity line of boundary conditions
   * 
   * @param ws
   * @param rootFeature
   */
  public StringBuffer createLineBC( GMLWorkspace ws, Feature rootFeature, URL file2d, URL schema2d )
  {
    Feature lineCollectionFE = ws.resolveLink( rootFeature, "lineCollectionMember" );
    List list = (List)lineCollectionFE.getProperty( "lineMember" );
    StringBuffer sb = new StringBuffer();
    sb.append( "           read(iin,'(60i5)')(line(j,k),k=1,60)" + "\n" );
    sb.append( "   NCL Kontinuitaetslinien (d.h. Knotennummern der Linien)" + "\n" );

    try
    {
      //	        URL gmlURL = new File(file2d).toURL();
      //	        URL schemaUrl = new File(schema2d).toURL();
      //	        GMLWorkspace ws2d = GmlSerializer.createGMLWorkspace(gmlURL, schemaUrl);
      GMLWorkspace ws2d = GmlSerializer.createGMLWorkspace( file2d, schema2d );
      final Feature rootFeature2d = ws2d.getRootFeature();

      for( int i = 0; i < list.size(); i++ )
      {
        StringBuffer sb2 = new StringBuffer();
        Feature fe = (Feature)list.get( i );
        if( fe != null )
        {
          GM_Curve curve = (GM_Curve)fe.getProperty( "listLinePos" );
          GM_LineString asLineString = curve.getAsLineString();
          GM_Position[] positions = asLineString.getPositions();

          StringWriter stringWriter = new StringWriter();
          PrintWriter printWriter = new PrintWriter( stringWriter );
          String format = "%5d";
          Object[] o;

          for( int j = 0; j < positions.length; j++ )
          {
            int id = getId( ws2d, rootFeature2d, positions[j].getX(), positions[j].getY() );

            o = new Object[]
            { new Integer( id ), };

            try
            {
              Format.fprintf( printWriter, format + "\n", o );
            }
            catch( IOException e1 )
            {
              e1.printStackTrace();
            }

          }

          sb2.append( stringWriter.getBuffer() );
          printWriter.close();
          try
          {
            stringWriter.close();
          }
          catch( IOException e )
          {
            e.printStackTrace();
          }
          sb.append( sb2 + "\n" );
        }
      }
    }
    catch( Exception e )
    {
      System.out.println( "error in generating continuity line" );
      e.printStackTrace();
    }
    sb.append( "            IQGEN Zeilen, die an der jeweiligen Kontinuitaetslinie j" + "\n" );
    sb.append( "            einen Durchfluss qf in der Richtung qdir vorgeben" + "\n" );

    return sb;
  }

  /**
   * returns id of node with particular coordinates in continuity line
   * 
   * @param ws
   * @param rootFeature
   * @param x
   * @param y
   */
  private int getId( GMLWorkspace ws, Feature rootFeature, double x, double y )
  {
    int id = 0;
    FEMNodes nodes = new FEMNodes();
    nodes.createNodeProperties( ws, rootFeature );
    ArrayList nodeList = nodes.getNodeList();

    for( Iterator iter = nodeList.iterator(); iter.hasNext(); )
    {
      Node n = (Node)iter.next();
      double xNode = n.getX();
      double yNode = n.getY();
      if( xNode == x && yNode == y )
      {
        id = n.getID();
        break;
      }
    }

    return id;
  }

}
