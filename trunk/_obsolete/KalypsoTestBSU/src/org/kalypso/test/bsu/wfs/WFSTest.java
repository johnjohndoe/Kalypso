/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.bsu.wfs;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.gmlschema.GMLSchemaTest;
import org.kalypso.ogc.wfs.IWFSLayer;
import org.kalypso.ogc.wfs.WFService;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class WFSTest extends TestCase
{
  final File testBase = new File( "F:/eclipse3.1/testBase" );

  /**
   *  
   */
  public void testBSU( ) throws Exception
  {
    final List<QName> ignoreFT = new ArrayList<QName>();
    // diese sind getested und ok:
    // ignoreFT.add( "EZG_Kollau" );
    // ignoreFT.add( "FFH-Flaechen" );
    // ignoreFT.add( "flows_pilotgebiete" );
    // ignoreFT.add( "DIP_lang_Textplaene_lang" );
    // ignoreFT.add( "flurstuecke" );
    // ignoreFT.add( "gemarkungen" );
    // ignoreFT.add( "DIP_b_tb_d_plaene_lang" );
    // ignoreFT.add( "gruenplan_2004" );
    // ignoreFT.add( "vsiegel1999" );
    // ignoreFT.add( "D9F17D6F-488E-48A6-BA32-D5F6EC8F47F0" );
    // ignoreFT.add( "DIP_lang_scheinblock_lang:" );
    // ignoreFT.add( "sip_einfach_pegelklassen" );
    // ignoreFT.add( "fisbo_bodhorz" );
    // ignoreFT.add( "DIP_lang_SpezialNutzung_lang" );
    // ignoreFT.add( "stadtteile" );
    // ignoreFT.add( "umriss" );
    ignoreFT.add( new QName( "wasserschutzgebiete" ) );
    // ignoreFT.add( "EZG_Tarpenbek" );
    // ignoreFT.add( "sip_nord" );
    // ignoreFT.add( "CDD2947D-EE17-4098-A4D0-107E9433173F" );
    // ignoreFT.add( "fisbo_bodkopf" );

    ignoreFT.add( new QName( "bezirke" ) ); // getFeature schliesst nicht ab
    ignoreFT.add( new QName( "MST_ezg_kollau_tarpenbek" ) );// geht nicht
    // ignoreFT.add( "gewaesserguete99_flaechen" );
    // ignoreFT.add( "ortsteile" );
    // ignoreFT.add( "DIP_lang_scheinblock_lang" );

    // hat falsches schema: (flurstuecke)
    ignoreFT.add( new QName( "gebaeude" ) );

    // gleicher complextype und element name
    ignoreFT.add( new QName( "DIP_lang_flaechennutzung_lang" ) );
    // shape kaputt -> Rainer G.
    // vermutlich < oder > in einem String-element
    ignoreFT.add( new QName( "855E4B54-7392-480D-8079-2DF593498C61" ) );
    ignoreFT.add( new QName( "50977EBA-D83C-4561-A9DF-38EE609F7BA6" ) ); // schutzgebiete

    // 
    ignoreFT.add( new QName( "gruenplan_2006" ) );
    ignoreFT.add( new QName( "WSG_in_ezg_kollau_tarpenbek" ) );

    final String baseURLAsString = "http://bsu-uio:8081/deegreewfs/wfs";

    tryToCreateWorkspaces( baseURLAsString, ignoreFT );

  }

  public void xxxtestXPlanung( )
  {
    // PROXY litzu135.lit.hamburg.de:8080
    // System.setProperty( "proxySet", "true" );
    // System.setProperty( "proxyHost", "litzu135.lit.hamburg.de" );
    // System.setProperty( "proxyPort", "8080" );

    final String baseURLAsString = "http://212.23.140.234/xplanung/ogcwebservice";
    final List<QName> ignoreFT = new ArrayList<QName>();
    tryToCreateWorkspaces( baseURLAsString, ignoreFT );
  }

  private void tryToCreateWorkspaces( final String baseURLAsString, final List<QName> ignoreFT )
  {
    System.out.println( "test: " + baseURLAsString );
    final StringBuffer errors = new StringBuffer();
    final StringBuffer ok = new StringBuffer();
    try
    {
      final URL baseURL = new URL( baseURLAsString );
      final String maxFeaturesAsString = "3";
      WFService wfs = new WFService(baseURL.toExternalForm());
      final IWFSLayer[] featureTypes = wfs.getLayer();
      for( final IWFSLayer ft : featureTypes )
      {
        final QName featureTypeQName = ft.getQName();
        System.out.println( "Teste TypeName: " + featureTypeQName );
        if( ignoreFT.contains( featureTypeQName ) )
        {
          System.out.println( ".. soll ignoriert werden" );
          continue;
        }
        try
        {
          final URL schemaURL = wfs.buildDescribeURLForFeatureType( featureTypeQName );
          // final URL schemaURL = WFSUtilities.createDescribeFeatureTypeRequestURL( capabilites, featureTypeQName );
          System.out.println( "teste parsen des Schemas von " + schemaURL.toString() );
          // final GMLSchema schema = new GMLSchema( schemaURL );
          final String testFileName = schemaURL.toString().replaceAll( "(\\(|\\)|\\\\|/|\\.|,|=|&|:|\\?)", "" ) + ".txt";
          final File testResource = new File( testBase, testFileName );
          boolean writeCompareFile = false;
          if( !testResource.exists() )
            writeCompareFile = true;

          GMLSchemaTest.loadAndTestSchema( schemaURL, testResource.toURL(), writeCompareFile );
          // hack
          // File file = new File( "D:/eclipse3.1/documents/schemaFromXPlanXSF.xsd" );
          // GMLSchemaTest.loadAndTestSchema( file.toURL(), testResource.toURL(), writeCompareFile );

          System.out.println( "schema erfolgreich geladen" );

          System.out.println( "teste laden eines GMLWorkspaces..." );
          final GMLWorkspace workspace = wfs.createGMLWorkspaceFromGetFeature( featureTypeQName, null, null, maxFeaturesAsString );
          // System.out.println( ".. workspace erfolgreich geladen" );
          final Feature rootFeature = workspace.getRootFeature();
          final FeatureList property = (FeatureList) rootFeature.getProperty( "featureMember" );
          final int size = property.size();
          if( size != 3 )
            errors.append( featureTypeQName ).append( ": Warnung nur " ).append( size ).append( " Feature im" + "Workspace " ).append( "\n" );
          else
            ok.append( featureTypeQName ).append( "\n" );
        }
        catch( final Exception e )
        {
          System.out.println( "Fehler bei Typename: " + featureTypeQName );
          e.printStackTrace();
          fail();
          errors.append( featureTypeQName ).append( ": Fehler bei Typename: " ).append( "\n" );
        }

      }
      System.out.println( "fertig" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      System.out.println( "Zusammenfassung:" );
      System.out.println( "Fehler:\n" + errors.toString() );
      System.out.println( "\nOK:\n" + ok.toString() );
    }
  }
}
