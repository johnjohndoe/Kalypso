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
package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author Gernot Belger
 */
public class ModifyFeatureGeometryCommand implements ICommand
{
  private final List<Handle> m_handles;

  private final double[] m_translation;

  private final double[] m_undoTranslation;

  private final GMLWorkspace m_workspace;

  /**
   * @param workspace
   * @param targetFeatures
   *          features to modify
   */
  public ModifyFeatureGeometryCommand( final GMLWorkspace workspace, final List<Handle> handles, final double translation[] )
  {
    m_workspace = workspace;
    m_handles = handles;
    m_translation = translation;
    m_undoTranslation = new double[translation.length];
    for( int i = 0; i < translation.length; i++ )
      m_undoTranslation[i] = -translation[i];
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    doIt( false );
  }

  private void doIt( final boolean undo )
  {
    final List<Feature> feList = new ArrayList<Feature>();

    for( final Handle handle : m_handles )
    {
      final GM_Position position = handle.getPosition();
      if( undo )
        position.translate( m_undoTranslation );
      else
        position.translate( m_translation );

      /* Reset the geometry value in order to invalidate the feature's envelope */
      final Feature feature = handle.getFeature();
      final IValuePropertyType propertyType = handle.getPropertyType();
      final GM_Object value = (GM_Object) feature.getProperty( propertyType );
      value.invalidate();
      feature.setProperty( propertyType, value );

      if( !feList.contains( feature ) )
        feList.add( feature );
    }

    m_workspace.fireModellEvent( new FeaturesChangedModellEvent( m_workspace, feList ) );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    doIt( false );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    doIt( true );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Geometrie ändern";
  }
}
