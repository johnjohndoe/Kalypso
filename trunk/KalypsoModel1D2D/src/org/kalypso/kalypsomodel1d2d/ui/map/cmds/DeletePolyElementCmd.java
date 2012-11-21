/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import gnu.trove.THashSet;

import java.util.Set;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Command for deleting one element. The change event has to fired from outside!
 * 
 * @author Patrice Congo
 */
public class DeletePolyElementCmd implements IFeatureChangeCommand
{
  private final Set<IPolyElement> m_elementsToRemove = new THashSet<>();

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d )
  {
    this( model1d2d, null );
  }

  public DeletePolyElementCmd( final IFEDiscretisationModel1d2d model1d2d, final IPolyElement pFeature )
  {
    m_model1d2d = model1d2d;
    addElementToRemove( pFeature );
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    for( final IPolyElement poly : m_elementsToRemove )
    {
      // delete link to complex elements
      final IFE1D2DComplexElement[] parentComplexElements = poly.getLinkedElements();
      for( final IFE1D2DComplexElement complexElement : parentComplexElements )
        complexElement.removeLinkedItem( poly );
    }

    // delete elements from model
    m_model1d2d.removeAllElements( m_elementsToRemove );
  }

  @Override
  public void redo( ) throws Exception
  {

  }

  @Override
  public void undo( ) throws Exception
  {

  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return m_elementsToRemove.toArray( new Feature[m_elementsToRemove.size()] );
  }

  public void addElementToRemove( final IPolyElement pFeature )
  {
    if( pFeature != null )
      m_elementsToRemove.add( pFeature );
  }
}