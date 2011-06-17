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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.gmlschema.feature.IFeatureType;

/**
 * @author doemming
 */
public class EditRelationOptionsContentProvider implements ITreeContentProvider
{
  private EditRelationInput m_input;

  @Override
  public void dispose( )
  {
    // nothing to do
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    if( newInput instanceof EditRelationInput )
      m_input = (EditRelationInput) newInput;
    else
      m_input = null;
  }

  @Override
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement instanceof EditRelationInput )
      return ((EditRelationInput) inputElement).getElements();

    return new Object[0];
  }

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IFeatureType )
      return m_input.getChildren( parentElement );

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  @Override
  public boolean hasChildren( final Object element )
  {
    if( element == null )
      return false;

    return getChildren( element ).length > 0;
  }

  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof IEditRelationType )
      return m_input.getParent( (IEditRelationType) element );

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }
}