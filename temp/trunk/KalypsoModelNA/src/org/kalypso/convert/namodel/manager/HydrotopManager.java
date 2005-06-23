package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

public class HydrotopManager extends AbstractManager
{

  private final NAConfiguration m_conf;

  private final FeatureType m_hydrotopFT;

  /*
   * 
   * @author huebsch
   */
  public HydrotopManager( GMLSchema schema, GMLSchema hydrotopSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getHydrotopFormatURL() );
    //    m_crs = crs;
    m_conf = conf;
    m_hydrotopFT = hydrotopSchema.getFeatureType( "Hydrotop" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {

    return null;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace hydWorkspace, GMLWorkspace modelWorkspace )
      throws Exception
  {
    //Catchment
    Feature modelRootFeature = modelWorkspace.getRootFeature();
    Feature modelCol = (Feature)modelRootFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)modelCol.getProperty( "catchmentMember" );

    Iterator catchmentIter = catchmentList.iterator();
    // vollständige HydrotopList
    Feature rootFeature = hydWorkspace.getRootFeature();
    Feature hydCol = (Feature)rootFeature.getProperty( "HydrotopCollectionMember" );
    FeatureList hydList = (FeatureList)hydCol.getProperty( "HydrotopMember" );
    Date calcDate = new Date();
    asciiBuffer.getHydBuffer().append( "Hydrotope Modell, Datum " + calcDate.toString() + "\n" );

    while( catchmentIter.hasNext() )
    {
      double versFlaeche = 0.0;
      double natFlaeche = 0.0;
      double gesFlaeche = 0.0;
      List hydWriteList = new ArrayList();
      final Feature catchmentFE = (Feature)catchmentIter.next();
      //      GM_Surface tGGeomProp = (GM_Surface)catchmentFE.getProperty( "Ort" );
      GM_Object tGGeomProp = (GM_Object)catchmentFE.getProperty( "Ort" );
      // Hydrotope im TeilgebietsEnvelope
      List hydInEnvList = hydList.query( catchmentFE.getEnvelope(), null );
      Iterator hydInEnvIter = hydInEnvList.iterator();
      //      System.out.println( "Teilgebiet Nummer:" + catchmentFE.getProperty(
      // "inum" ) );
      while( hydInEnvIter.hasNext() )
      {
        final Feature hydFeature = (Feature)hydInEnvIter.next();
        final GM_Object hydGeomProp = (GM_Object)hydFeature.getProperty( "Ort" );

        //TODO: der centroid muss nicht zwangsläufig im teilgebiet sein zu
        // welchem das hydrotop gehört!!! (wie kann man das besser abfragen)
        //        if( tGGeomProp.contains( hydGeomProp) )
        if( GeometryUtilities.isInside( tGGeomProp, hydGeomProp ) )
        {
          hydWriteList.add( hydFeature );
          double hydGesFlaecheTest = ( (Double)hydFeature.getProperty( "flaech" ) ).doubleValue();
          double hydGesFlaeche = GeometryUtilities.calcArea( hydGeomProp );
          double versGrad = ( (Double)hydFeature.getProperty( "m_vers" ) ).doubleValue();
          double korVersGrad = ( (Double)hydFeature.getProperty( "fak_vers" ) ).doubleValue();
          if( korVersGrad < 0.0 )
          {
            // TODO evt. beim NAImport setzen
            korVersGrad = 1.0;
          }
          double gesVersGrad = ( versGrad * korVersGrad );
          versFlaeche += ( hydGesFlaeche * gesVersGrad );
          natFlaeche += ( hydGesFlaeche - ( hydGesFlaeche * gesVersGrad ) );
          gesFlaeche += hydGesFlaeche;
        }

      }

      int hydAnzahl = hydWriteList.size();

      double tGArea = GeometryUtilities.calcArea( tGGeomProp );

      if( (int)tGArea != (int)gesFlaeche )
      {
        System.out.println( "Fehler in den Hydrotopen!" );
        double fehler = ( tGArea - gesFlaeche );
        System.out.println( "Fläche Teilgebiet (Nummer:" + catchmentFE.getProperty( "inum" ) + ") (" + (int)tGArea
            + ") entspricht nicht der Summe der Hydrotopflächen (" + (int)gesFlaeche + ") Fehler :"
            + ( fehler / gesFlaeche * 100d ) + "% diff: " + fehler );
      }
      else
        System.out.println( "OK Fläche Teilgebiet (Nummer:" + catchmentFE.getProperty( "inum" ) );
      asciiBuffer.getHydBuffer().append(
          FortranFormatHelper.printf( FeatureHelper.getAsString( catchmentFE, "inum" ), "i4" ) );
      asciiBuffer.getHydBuffer().append(
          " " + hydAnzahl + " " + (int)versFlaeche + " " + (int)natFlaeche + " " + (int)gesFlaeche + "\n" );

      Iterator hydIter = hydWriteList.iterator();
      int anzHydrotope = 0;
      while( hydIter.hasNext() )
      {
        anzHydrotope += 1;
        final Feature hydrotopFE = (Feature)hydIter.next();
        writeFeature( asciiBuffer, hydrotopFE, anzHydrotope );
      }
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, Feature feature, int anzHydrotope ) throws Exception
  {
    double HGesFlaeche = ( (Double)feature.getProperty( "flaech" ) ).doubleValue();
    double HVersGrad = ( (Double)feature.getProperty( "m_vers" ) ).doubleValue();
    double HKorVersGrad = ( (Double)feature.getProperty( "fak_vers" ) ).doubleValue();
    if( HKorVersGrad == 0.0 )
    {
      HKorVersGrad = 1.0;
    }
    int natHFlaeche = (int)( HGesFlaeche - ( HGesFlaeche * HVersGrad * HKorVersGrad ) );
    //  3
    StringBuffer b = new StringBuffer();
    b.append( FortranFormatHelper.printf( Integer.toString( natHFlaeche ), "a10" ) );
    b.append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "nutz" ), "a10" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "boden" ), "a10" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "m_perkm" ), "*" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "m_f1gws" ), "*" ) );
    b.append( " " + "0.000" );
    b.append( " " + "10.000" );
    b.append( " " + "0.27" );
    b.append( " " + "0.000" );
    b.append( " " + anzHydrotope );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "m_vers" ), "*" ) );

    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "fak_vers" ), "*" ) );

    asciiBuffer.getHydBuffer().append( b.toString() + "\n" );
  }

}
