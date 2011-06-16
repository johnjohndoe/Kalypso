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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.Collection;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.internal.wspm.WaterBodyTreeNode;

/**
 * @author Gernot Belger
 */
public class ByWaterBodyContentProvider implements ITreeContentProvider
{
  @Override
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement instanceof ConnectionInput )
    {
      final WaterBodyTreeNode rootNode = ((ConnectionInput) inputElement).getRootNode();
      final Object[] allChildren = rootNode.getAllChildren();
      if( ArrayUtils.isEmpty( allChildren ) )
        return new Object[] { PdbLabelProvider.EMPTY_WATER_BODY };

      return allChildren;
    }

    if( inputElement instanceof Object[] )
      return (Object[]) inputElement;

    if( inputElement instanceof Collection )
      return ((Collection< ? >) inputElement).toArray();

    if( inputElement != null )
      return new Object[] { inputElement };

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  @Override
  public boolean hasChildren( final Object element )
  {
    if( element instanceof WaterBodyTreeNode )
    {
      final WaterBodyTreeNode node = (WaterBodyTreeNode) element;
      final Object[] allChildren = node.getAllChildren();
      return !Arrays.isEmpty( allChildren );
    }

    if( element instanceof State )
      return false;

    return false;
  }

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof WaterBodyTreeNode )
    {
      final WaterBodyTreeNode node = (WaterBodyTreeNode) parentElement;
      final Object[] allChildren = node.getAllChildren();
      return allChildren;
    }

    return ArrayUtils.EMPTY_OBJECT_ARRAY;
  }

  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof CrossSection )
      return ((CrossSection) element).getWaterBody();

    return null;
  }

  @Override
  public void dispose( )
  {
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
  }
}