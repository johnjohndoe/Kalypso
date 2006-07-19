/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ogc.gml.command;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.MultiUrlCatalog;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.TypeHandlerUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.UrlCatalogOGC;

/**
 * This class tests the relative feature change on some test data
 * 
 * @author skurzbach
 */
@SuppressWarnings("unchecked")
public class RelativeFeatureChangeTest extends TestCase
{
  private static final int COMMAND_COUNT = 5;
  
  private final Feature m_original;

  private final IPropertyType[] m_originalProperties;

  private final List<FeatureChange>[] m_changes = new List[COMMAND_COUNT];

  private final Feature[] m_result = new Feature[COMMAND_COUNT];

  private final CommandableWorkspace m_workspace;

  private final ITypeRegistry<IMarshallingTypeHandler> m_typeRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

  /**
   * Initializes some features for the test
   */
  public RelativeFeatureChangeTest( ) throws Exception
  {
    TypeHandlerUtilities.registerXSDSimpleTypeHandler( m_typeRegistry );
    TypeHandlerUtilities.registerTypeHandlers( m_typeRegistry );

    final MultiUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[] { new UrlCatalogOGC(), new DeegreeUrlCatalog() } );
    final File cacheDirectory = FileUtilities.createNewTempDir( "kalypsoschemacache" );
    if( !cacheDirectory.exists() )
      cacheDirectory.mkdirs();
    cacheDirectory.deleteOnExit();
    GMLSchemaCatalog.init( catalog, cacheDirectory );

    final URL modelFile = getClass().getResource( "resources/testFeature.gml" );
    m_workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( modelFile ) );

    final Feature m_feature = m_workspace.getRootFeature();
    final FeatureList featureList = (FeatureList) m_feature.getProperty( 0 );

    m_original = (Feature) featureList.get( 0 );
    for( int i = 0; i < COMMAND_COUNT; i++ )
    {
      m_result[i] = (Feature) featureList.get( i + 1 );
      m_changes[i] = new ArrayList<FeatureChange>();
    }

    m_originalProperties = m_original.getFeatureType().getProperties();
    
    // Command runs in the order add, multiply, subtract, divide, set that work on the same feature. 
    for( IPropertyType propertyType : m_originalProperties )
    {
      if( RelativeFeatureChange.isNumeric( propertyType ) )
      {
        // add 2.5 to value
        final RelativeFeatureChange fcADD = new RelativeFeatureChange( m_original, (IValuePropertyType) propertyType, "+", 2.5 );
        m_changes[0].add( fcADD );
        // multiply value by 2.5
        final RelativeFeatureChange fcMULTIPLY = new RelativeFeatureChange( m_original, (IValuePropertyType) propertyType, "*", 2.5 );
        m_changes[1].add( fcMULTIPLY );
        // subtract 2.5 from value
        final RelativeFeatureChange fcSUBTRACT = new RelativeFeatureChange( m_original, (IValuePropertyType) propertyType, "-", 2.5 );
        m_changes[2].add( fcSUBTRACT );
        // divide value by 2.5
        final RelativeFeatureChange fcDIVIDE = new RelativeFeatureChange( m_original, (IValuePropertyType) propertyType, "/", 2.5 );
        m_changes[3].add( fcDIVIDE );
        // set all values to 1.0
        final RelativeFeatureChange fcSET = new RelativeFeatureChange( m_original, (IValuePropertyType) propertyType, "=", 1.0 );
        m_changes[4].add( fcSET );
      }
    }
  }

  /**
   * Test method for 'org.kalypso.ui.editor.actions.RelativeFeatureChange.getNewValue()'
   * The expected results are in the file resources/testFeature.gml.
   */
  public final void testGetNewValue( ) throws Exception
  {
    for( int i = 0; i < COMMAND_COUNT; i++ )
    {
      // apply changes
      final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( m_workspace, m_changes[i].toArray( new FeatureChange[m_changes[i].size()] ) );
      m_workspace.postCommand( changeFeaturesCommand );

      for( IPropertyType propertyType : m_originalProperties )
      {
        if( RelativeFeatureChange.isNumeric( propertyType ) )
        {
          final Object propertyExpected = m_result[i].getProperty( propertyType );
          final Object propertyResult = m_original.getProperty( propertyType );
          assertEquals( propertyExpected.getClass(), propertyResult.getClass() );
          assertEquals( propertyExpected, propertyResult );
        }
      }
    }
  }
}
