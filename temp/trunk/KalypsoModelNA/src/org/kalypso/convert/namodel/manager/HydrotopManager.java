package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.net.URL;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Geometry;

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

/**
 * @author huebsch
 */
public class HydrotopManager extends AbstractManager
{
  final NAConfiguration m_conf;

  final Hashtable<String, Double> m_landuseMap = new Hashtable<String, Double>();

  public HydrotopManager( NAConfiguration conf ) throws IOException
  {
    super( conf.getHydrotopFormatURL() );
    m_conf = conf;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  public String mapID( int id, IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( URL url ) throws Exception
  {

    return null;
  }

  public void writeFile( AsciiBuffer asciiBuffer, GMLWorkspace hydWorkspace, GMLWorkspace modelWorkspace, GMLWorkspace parameterWorkspace ) throws Exception
  {
    final IDManager idManager = m_conf.getIdManager();
    // Catchment
    Feature modelRootFeature = modelWorkspace.getRootFeature();
    Feature modelCol = (Feature) modelRootFeature.getProperty( new QName( NaModelConstants.NS_NAMODELL, "CatchmentCollectionMember" ) );
    List catchmentList = (List) modelCol.getProperty( new QName( NaModelConstants.NS_NAMODELL, "catchmentMember" ) );

    Feature parameterRootFeature = parameterWorkspace.getRootFeature();
    List landuseList = (List) parameterRootFeature.getProperty( new QName( NaModelConstants.NS_NAPARAMETER, "landuseMember" ) );
    Iterator landuseIter = landuseList.iterator();
    while( landuseIter.hasNext() )
    {
      Feature landuseFE = (Feature) landuseIter.next();
      String landuseName = (String) landuseFE.getProperty( new QName( XMLHelper.GMLSCHEMA_NS, NaModelConstants.GML_FEATURE_NAME_PROP ) );

      final IRelationType rt = (IRelationType) landuseFE.getFeatureType().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK ) );
      Feature linkedSealingFE = parameterWorkspace.resolveLink( landuseFE, rt );
      Double SealingRate = (Double) linkedSealingFE.getProperty( new QName( NaModelConstants.NS_NAPARAMETER, NaModelConstants.PARA_LANDUSE_PROP_SEALING ) );
      if( !m_landuseMap.containsKey( landuseName ) )
        m_landuseMap.put( landuseName, SealingRate );
      // TODO: Errormassages in logger
      else
        System.out.println( "Der Landnutzungstyp " + landuseName + "existiert mehrfach.\n Überprüfen Sie die Landnutzungstypen." );
    }

    Iterator catchmentIter = catchmentList.iterator();
    // vollständige HydrotopList
    Feature rootFeature = hydWorkspace.getRootFeature();
    FeatureList hydList = (FeatureList) rootFeature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_MEMBER ) );
    asciiBuffer.getHydBuffer().append( "Hydrotope aus GML\n" );

    while( catchmentIter.hasNext() )
    {
      final Feature catchmentFE = (Feature) catchmentIter.next();
      if( asciiBuffer.writeFeature( catchmentFE ) ) // do it only for relevant catchments
      {

        Comparator comparator = new Comparator()
        {
          public int compare( Object o1, Object o2 )
          {
            String id1 = ((Feature) o1).getId();
            String id2 = ((Feature) o2).getId();
            return id1.compareTo( id2 );
          }
        };

        TreeSet<Object> hydWriteSet = new TreeSet<Object>( comparator );

        double versFlaeche = 0.0;
        double natFlaeche = 0.0;
        double gesFlaeche = 0.0;
        // final List hydWriteList = new ArrayList();
        final GM_Object tGGeomProp = (GM_Object) catchmentFE.getProperty( new QName( NaModelConstants.NS_NAMODELL, NaModelConstants.CATCHMENT_GEOM_PROP ) );

        // Hydrotope im TeilgebietsEnvelope
        final List hydInEnvList = hydList.query( catchmentFE.getEnvelope(), null );
        final Iterator hydInEnvIter = hydInEnvList.iterator();
        while( hydInEnvIter.hasNext() )
        {
          final Feature hydFeature = (Feature) hydInEnvIter.next();
          final GM_Object hydGeomProp = (GM_Object) hydFeature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_GEOM ) );
          // Hint: JavaTopologySuite has no Coordinate System (here: all geometries
          // are in the same cs - see NaModelInnerCalcJob)
          final Geometry jtsTG = JTSAdapter.export( tGGeomProp );
          final Geometry jtsHyd = JTSAdapter.export( hydGeomProp );
          if( jtsTG.contains( jtsHyd.getInteriorPoint() ) )
          {
            // hydWriteList.add( hydFeature );
            hydWriteSet.add( hydFeature );
            double hydGesFlaeche = GeometryUtilities.calcArea( hydGeomProp );
            String landuse = (String) hydFeature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ) );
            double versGrad = m_landuseMap.get( landuse ).doubleValue();
            double korVersGrad = ((Double) hydFeature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ) )).doubleValue();
            double gesVersGrad = (versGrad * korVersGrad);
            versFlaeche += (hydGesFlaeche * gesVersGrad);
            natFlaeche += (hydGesFlaeche - (hydGesFlaeche * gesVersGrad));
            gesFlaeche += hydGesFlaeche;
          }

        }

        // int hydAnzahl = hydWriteList.size();
        int hydAnzahl = hydWriteSet.size();

        double tGArea = GeometryUtilities.calcArea( tGGeomProp );
        // TODO: throw exception (to the user), if writing of hydrotope file is checked (testing!!!)
        double fehler = (tGArea - gesFlaeche);
        int fehlerinProzent = (int) Math.abs( fehler / gesFlaeche * 100d );
        // if( (int)tGArea != (int)gesFlaeche )
        if( fehlerinProzent > 1 )
        {
          System.out.println( "Fehler in den Hydrotopen!" );
          System.out.println( "Fläche Teilgebiet (ID:" + catchmentFE.getId() + ") (" + (long) tGArea + ") entspricht nicht der Summe der Hydrotopflächen (" + (long) gesFlaeche + ") Fehler : "
              + fehlerinProzent + "%, diff: " + ((int) fehler) );
          // TODO report error to user or log file
        }
        asciiBuffer.getHydBuffer().append( FortranFormatHelper.printf( idManager.getAsciiID( catchmentFE ), "i4" ) );
        asciiBuffer.getHydBuffer().append( " " + hydAnzahl + " " + (long) versFlaeche + " " + (long) natFlaeche + " " + (long) gesFlaeche + "\n" );

        Iterator hydIter = hydWriteSet.iterator();
        // Iterator hydIter = hydWriteList.iterator();
        int anzHydrotope = 0;
        while( hydIter.hasNext() )
        {
          anzHydrotope += 1;
          final Feature hydrotopFE = (Feature) hydIter.next();
          writeFeature( asciiBuffer, hydrotopFE, anzHydrotope );
        }
      }
    }
  }

  private void writeFeature( AsciiBuffer asciiBuffer, Feature feature, int anzHydrotope ) throws Exception
  {
    double HGesFlaeche = ((Double) feature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_AREA ) )).doubleValue();
    Double SealingRate = m_landuseMap.get( feature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_LANDUSE_NAME ) ) );
    double HVersGrad = (SealingRate).doubleValue();
    double HKorVersGrad = 1;
    try
    {
      HKorVersGrad = ((Double) feature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_SEAL_CORR_FACTOR ) )).doubleValue();
    }
    catch( Exception e )
    {
      System.out.println( "Der Korrekturfaktor des Versiegelungsgrades im Hydrotop (ID=" + feature.getId() + ") 0.0, er wird daher auf 1.0 gesetzt" );
    }
    long natHFlaeche = (long) (HGesFlaeche - (HGesFlaeche * HVersGrad * HKorVersGrad));
    // 3
    StringBuffer b = new StringBuffer();
    b.append( FortranFormatHelper.printf( Long.toString( natHFlaeche ), "a10" ) );
    b.append( FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "landuse" ), "a10" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "soiltype" ), "a10" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "m_perkm" ), "*" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "m_f1gws" ), "*" ) );
    b.append( " " + "0.000" );
    b.append( " " + "10.000" );
    b.append( " " + "0.27" );
    b.append( " " + "0.000" );
    b.append( " " + anzHydrotope );
    b.append( " " + FortranFormatHelper.printf( SealingRate.toString(), "*" ) );
    b.append( " " + FortranFormatHelper.printf( FeatureHelper.getAsString( feature, "corrSealing" ), "*" ) );

    final String hydType = (String) feature.getProperty( new QName( NaModelConstants.NS_NAHYDROTOP, NaModelConstants.HYDRO_PROP_HYDTYPE ) );
    int hydTypeNumber = 0;
    if( hydType == null || hydType.equals( "Bodenspeicher" ) )
      hydTypeNumber = 0;
    else if( hydType.equals( "MuldenRigole" ) )
      hydTypeNumber = 1;
    else if( hydType.equals( "Dachbegruenung" ) )
      hydTypeNumber = 2;
    else
      System.out.println( "Kein gültiger Hydrotoptyp" );
    b.append( " " + hydTypeNumber );
    asciiBuffer.getHydBuffer().append( b.toString() + "\n" );
  }
  /**
   * @param asciiBuffer
   * @param hydrotopeWorkspace
   * @param modelWorkspace
   * @param parameterWorkspace
   */

}
