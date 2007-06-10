/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.combinemodells;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import junit.framework.TestCase;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.FidCollectorVisitor;

/**
 * @author huebsch
 */

public class CombineModells extends TestCase
{
  public void testCombineModells( ) throws Exception
  {
    final CombineModells modell2 = new CombineModells();
    modell2.combineIt();
  }

  public CombineModells( ) throws Exception
  {

  }

  // TODO: this seems not to be used any more
  // if we delete this we may get rid of the id-map for the gml-writer...
  public void combineIt( ) throws Exception
  {
    final URL baseModel = getClass().getResource( "resources/base.gml" );
    final GMLWorkspace baseWorkspace = GmlSerializer.createGMLWorkspace( baseModel, null );

    final URL additiveModel = getClass().getResource( "resources/additive.gml" );
    final GMLWorkspace additiveWorkspace = GmlSerializer.createGMLWorkspace( additiveModel, null );

    final HashMap<String, String> IDBase = collectIDs( baseWorkspace );
    final HashMap<String, String> IDAdditive = collectIDs( additiveWorkspace );
    final HashMap<String, String> IDMapToChange = getIDsToChange( IDBase, IDAdditive );
    HashMap<String, String> IDMapChange = new HashMap<String, String>();
    if( IDMapToChange.size() >= 0 )
    {
      IDMapChange = changeIDs( IDMapToChange, additiveWorkspace, IDAdditive, IDBase );
    }

    final File tmpDir = new File( "C:\\tmp\\ModellCombination" );
    final File file = File.createTempFile( "newAdditiveModell", ".gml", tmpDir );
    final OutputStreamWriter writer = new FileWriter( file );
    GmlSerializer.serializeWorkspace( writer, additiveWorkspace, "UTF-8", IDMapChange );
    writer.close();
    System.out.println( " combined model is written to " + file.getCanonicalPath() );
  }

  private HashMap<String, String> changeIDs( final HashMap<String, String> mapChange, final GMLWorkspace additiveWorkspace, final HashMap<String, String> mapAdditive, final HashMap mapBase )
  {
    final HashMap<String, String> IDMap = new HashMap<String, String>();
    final Collection<String> ChangeIDCollection = mapChange.values();
    for( final Object element : ChangeIDCollection )
    {
      final String oldID = (String) element;
      final String FTName = additiveWorkspace.getFeature( oldID ).getFeatureType().getQName().getLocalPart();
      int i = 1;
      String newID = FTName + Integer.toString( i );
      while( mapAdditive.containsValue( newID ) || mapBase.containsValue( newID ) || IDMap.containsValue( newID ) )
      {
        i++;
        newID = FTName + Integer.toString( i );
      }
      IDMap.put( oldID, newID );
    }
    return IDMap;
  }

  private HashMap<String, String> getIDsToChange( final HashMap<String, String> base, final HashMap<String, String> additive )
  {
    final HashMap<String, String> equalIDs = new HashMap<String, String>();
    final Collection collection = additive.values();

    for( final Iterator iter = collection.iterator(); iter.hasNext(); )
    {
      final String checkID = (String) iter.next();
      if( base.containsValue( checkID ) )
      {
        equalIDs.put( checkID, checkID );
      }
    }
    return equalIDs;
  }

  private void getNewID( final GMLWorkspace baseWorkspace, final HashMap<String, String> mapBase, final GMLWorkspace additiveWorkspace, final HashMap<String, String> mapAdditive, String checkID )
  {
    final String FTName = (additiveWorkspace.getFeature( checkID )).getFeatureType().getQName().getLocalPart();
    int i = 1;
    while( mapBase.containsKey( checkID ) || mapAdditive.containsKey( checkID ) )
    {
      i++;
      checkID = FTName + Integer.toString( i );
    }
  }

  private HashMap<String, String> collectIDs( final GMLWorkspace workspace )
  {
    final HashMap<String, String> IDMap = new HashMap<String, String>();
    final Feature RootFE = workspace.getRootFeature();
    final FidCollectorVisitor IDVisitor = new FidCollectorVisitor();
    workspace.accept( IDVisitor, RootFE, 1 );
    final String[] results = IDVisitor.getResults( true );
    for( final String element : results )
    {
      IDMap.put( element, element );
    }
    return IDMap;
  }

}