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
package org.kalypso.model.wspm.ui.view.table.swt;

import java.util.List;

import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Item;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;

/**
 * @author Gernot Belger
 */
public class ProfilCellModifier implements ICellModifier, ICellEditorValidator
{
  private final TableViewer m_viewer;

  public ProfilCellModifier( final TableViewer viewer )
  {
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    if( pem == null )
      return false;

    return propertyForID( pem.getProfil(), property ) != null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    return String.format( ProfilLabelProvider.ENTRY_FORMAT, getValueAsDouble( element, property ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    if( pem == null )
      return;

    final IProfilPoint point;
    if( element instanceof Item )
      point = (IProfilPoint) ((Item) element).getData();
    else if( element instanceof IProfilPoint )
      point = (IProfilPoint) element;
    else
      point = null;

    final POINT_PROPERTY col = propertyForID( pem.getProfil(), property );

    try
    {
      final double oldValue = point.getValueFor( col );
      /* Null values are not allowed for the profile. */
      if( value == null || isValid( value ) != null )
        return;

      final Double newValue = parseValue( value );

      if( Double.compare( oldValue, newValue ) != 0 )
      {
        final PointPropertyEdit[] profilChanges = new PointPropertyEdit[] { new PointPropertyEdit( point, col, newValue ) };
        final ProfilOperation operation = new ProfilOperation( "", pem, profilChanges, true );
        new ProfilOperationJob( operation ).schedule();
      }
    }
    catch( final Exception t )
    {
      // ignore, set null?
      t.printStackTrace();
    }
  }

  private Double parseValue( final Object value )
  {
    if( value == null )
      return null;

    return new Double( value.toString().replace( ',', '.' ) );
  }

  public static POINT_PROPERTY propertyForID( final IProfil profil, final String idstr )
  {
    final List<POINT_PROPERTY> columnKeys = profil.getPointProperties( true );
    for( final POINT_PROPERTY key : columnKeys )
    {
      final POINT_PROPERTY pointProperty = key;
      final String id = pointProperty.toString();
      if( id.equals( idstr ) )
        return pointProperty;
    }

    return null;
  }

  public double getValueAsDouble( final Object element, final String property )
  {
    final IProfilPoint row = (IProfilPoint) element;
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    final POINT_PROPERTY col = propertyForID( pem.getProfil(), property );

    try
    {
      return row.getValueFor( col );
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();

      return Double.NaN;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object value )
  {
    try
    {
      parseValue( value );
    }
    catch( final NumberFormatException nfe )
    {
      return "Keine Dezimalzahl: " + value;
    }
    catch( final Throwable t )
    {
      return t.getLocalizedMessage();
    }

    return null;
  }

}
