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
 * Created on 20.01.200
 */
package org.kalypso.convert.model2d;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import com.braju.format.Format;

/**
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class BCGeom
{

  /**
   * creates the second group of parameters for boundary conditions
   * 
   * @param ws
   * @param rootFeature
   */
  public StringBuffer createBCGeom( GMLWorkspace ws, Feature rootFeature )
  {
    Feature geomCollectionFE = ws.resolveLink( rootFeature, "geomCollectionMember" );
    List geomList = (List)geomCollectionFE.getProperty( "geomMember" );
    StringBuffer sb = new StringBuffer();
    for( Iterator iter = geomList.iterator(); iter.hasNext(); )
    {
      Feature geomFE = (Feature)iter.next();
      String blockNames = "     OMEGA     ,ELEV,   XSCALE,   ZSCALE,     DSET    ,DSETD,     UNOM,     HMIN   ,ASCALE      URFC";
      sb.append( blockNames + "\n" );

      StringWriter stringWriter = new StringWriter();
      PrintWriter printWriter = new PrintWriter( stringWriter );
      String format = "%10.2f%10.3f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f%10.2f";
      Object[] o = new Object[]
      {
          new Double( Double.parseDouble( "" + geomFE.getProperty( "OMEGA" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "ELEV" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "XSCALE" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "ZSCALE" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "DSET" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "DSETD" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "UNOM" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "HMIN" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "ASCALE" ) ) ),
          new Double( Double.parseDouble( "" + geomFE.getProperty( "URFC" ) ) ),

      };
      
      try
      {
        Format.fprintf( printWriter, format + "\n", o );
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }

      sb.append( stringWriter.getBuffer() );
      printWriter.close();
      try
      {
        stringWriter.close();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }

      sb.append( "x" + "\n" );
    }

    return sb;
  }

}
