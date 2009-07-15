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
package org.kalypso.model.wspm.ui.view.table;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.property.restriction.RestrictionUtilities;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiBooleanHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDateHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDecimalHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDoubleHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiEnumerationHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiIntegerHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiStringHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

/**
 * @author Dirk Kuch
 */
public class GenericComponentUiHandlerProvider implements IComponentUiHandlerProvider
{
  private final IProfil m_profile;

  public GenericComponentUiHandlerProvider( final IProfil profile )
  {
    m_profile = profile;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider#createComponentHandler(org.kalypso.observation.result.TupleResult)
   */
  public Map<Integer, IComponentUiHandler> createComponentHandler( final TupleResult tupleResult )
  {
    Assert.isTrue( tupleResult == m_profile.getResult() );

    final IComponent[] pointMarkerTypes = m_profile.getPointMarkerTypes();
    final IComponent[] components = m_profile.getPointProperties();

    final Map<Integer, IComponentUiHandler> handler = new LinkedHashMap<Integer, IComponentUiHandler>();

    handler.put( -1, new ComponentUiProblemHandler( m_profile ) );

    for( int i = 0; i < components.length; i++ )
    {
      final IComponent component = components[i];

      /* marker?!? yes -> continue */
      if( ArrayUtils.contains( pointMarkerTypes, component ) )
        continue;

      final int spacing = 100 / components.length;

      final IComponentUiHandler h = createHandler( i, component, spacing );
      handler.put( i, h );
    }

    return handler;
  }

  private IComponentUiHandler createHandler( final int index, final IComponent component, final int spacing )
  {
    final QName valueTypeName = component.getValueTypeName();

    final String label = component.getName();
    final IRestriction[] restrictions = component.getRestrictions();
    if( ComponentUtilities.restrictionContainsEnumeration( restrictions ) )
    {
      final Map<Object, IAnnotation> items = RestrictionUtilities.getEnumerationItems( restrictions );
      return new ComponentUiEnumerationHandler( index, true, true, true, label, SWT.LEFT, 100, spacing, "%s", "<not set>", items ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    if( XmlTypes.XS_DATETIME.equals( valueTypeName ) )
      return new ComponentUiDateHandler( index, true, true, true, label, SWT.NONE, 100, spacing, "%s", "%s", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_STRING.equals( valueTypeName ) )
      return new ComponentUiStringHandler( index, true, true, true, label, SWT.NONE, 100, spacing, "%s", "%s", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_INTEGER.equals( valueTypeName ) )
      return new ComponentUiIntegerHandler( index, true, true, true, label, SWT.NONE, 100, spacing, "%s", "%s", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_DECIMAL.equals( valueTypeName ) )
      return new ComponentUiDecimalHandler( index, true, true, true, label, SWT.RIGHT, 100, spacing, "%.04f", "", "%.04f" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_DOUBLE.equals( valueTypeName ) )
      return new ComponentUiDoubleHandler( index, true, true, true, label, SWT.RIGHT, 100, spacing, "%.04f", "", "%.04f" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_BOOLEAN.equals( valueTypeName ) )
      return new ComponentUiBooleanHandler( index, true, true, true, label, SWT.CENTER, 100, spacing, "%b", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    if( XmlTypes.XS_STRING.equals( valueTypeName ) )
      return new ComponentUiStringHandler( index, true, true, true, label, SWT.CENTER, 100, spacing, "%b", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    throw new UnsupportedOperationException();
  }

}
