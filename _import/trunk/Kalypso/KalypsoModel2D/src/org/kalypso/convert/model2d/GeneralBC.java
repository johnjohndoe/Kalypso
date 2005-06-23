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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

import com.braju.format.Format;

/**
 * ----------------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */
public class GeneralBC
{

  /**
   * creates general properties of boundary conditions
   * 
   * @param ws
   * @param rootFeature
   */
  public StringBuffer createGeneral2BC( GMLWorkspace ws, Feature rootFeature )
  {
    Feature generalCollectionFE = ws.resolveLink( rootFeature, "genCollectionMember" );
    List generalList = (List)generalCollectionFE.getProperty( "genMember" );
    //TODO new addtional param group
    StringBuffer sb = new StringBuffer();
    for( int i = 0; i < generalList.size(); i++ )
    {
      Feature generalFE = (Feature)generalList.get( i );
      //TODO why is generalFE == null?
      if( generalFE != null )
      {
        String blockNames = "  FEM     MORPH       P_BOTTOM             MINEDDY";
        String blockNames2 = "   AUSSTEU     ITURB           P_PRANDTL";
        sb.append( blockNames + "\n" );
        sb.append( blockNames2 + "\n" );

        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter( stringWriter );
        String format = "%5d%5d%5d%5d%10.3f%10.3f%10.1f";
        Object[] o = new Object[]
        {
            new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "FEM" ) ) ),
            new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "AUSSTEU" ) ) ),
            new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "MORPH" ) ) ),
            new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "ITURB" ) ) ),
            new Double( Double.parseDouble( "" + generalFE.getProperty( "P_BOTTOM" ) ) ),
            new Double( Double.parseDouble( "" + generalFE.getProperty( "P_PRANDTL" ) ) ),
            new Double( Double.parseDouble( "" + generalFE.getProperty( "MINEDDY" ) ) ),

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
    }

    return sb;
  }

  /**
   * 
   * @param ws
   * @param rootFeature
   */
  public StringBuffer createGeneralBC( GMLWorkspace ws, Feature rootFeature )
  {
    Feature generalCollectionFE = ws.resolveLink( rootFeature, "generalCollectionMember" );
    List generalList = (List)generalCollectionFE.getProperty( "generalMember" );

    StringBuffer sb = new StringBuffer();
    for( Iterator iter = generalList.iterator(); iter.hasNext(); )
    {
      Feature generalFE = (Feature)iter.next();
      String blockNames = "   NE NMAT  NPX  NBX NWID NSID IPRT  NCL       IRO     IQGEN    ISTGEN    IDNOPT";
      String blockNames2 = "                                        IWIND     IRSLP     IHGEN     NCFLW     IBGEN";

      StringWriter stringWriter = new StringWriter();
      PrintWriter printWriter = new PrintWriter( stringWriter );
      String format = "%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d%5d";
      Object[] o = new Object[]
      {
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NE" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NMAT" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NPX" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NBX" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NWID" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NSID" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IPRT" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NCL" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IWIND" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IRO" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IRSLP" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IQGEN" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IHGEN" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "ISTGEN" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "NCFLW" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IDNOPT" ) ) ),
          new Integer( (int)Double.parseDouble( "" + generalFE.getProperty( "IBGEN" ) ) ), };
      try
      {
        Format.fprintf( printWriter, format + "\n", o );
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
      sb.append( blockNames + "\n" );
      sb.append( blockNames2 + "\n" );

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
