/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.risk.widget;

import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.SWT;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnWidthInfo;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDecimalHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDoubleHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiStringHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;
import org.kalypso.risk.i18n.Messages;

/**
 * @author Gernot Belger
 */
final class StatisticResultComponentProvider implements IComponentUiHandlerProvider
{
  private static final String NUMBER_FORMAT = "%,.02f "; //$NON-NLS-1$

  @Override
  public Map<Integer, IComponentUiHandler> createComponentHandler( final TupleResult tupleResult )
  {
    final Map<Integer, IComponentUiHandler> myMap = new LinkedHashMap<>();

    final IComponent[] components = tupleResult.getComponents();

    int count = 0;
    for( final IComponent component : components )
    {
      final IComponentUiHandler handler = getHandler( component, count );
      if( handler != null )
        myMap.put( count++, handler );
    }

    return myMap;
  }

  private IComponentUiHandler getHandler( final IComponent component, final int componentIndex )
  {
    final String compName = component.getName();
    final String[] compNameStrings = compName.split( "_" ); //$NON-NLS-1$

    final String phenName = component.getPhenomenon().getName();

    final String nullFormat = StringUtils.EMPTY;

    switch( phenName )
    {
      case "TotalDamage": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.1", compNameStrings[1] ); //$NON-NLS-1$
        return new ComponentUiDecimalHandler( componentIndex, false, true, true, headerName, SWT.RIGHT, ColumnWidthInfo.PACK, -1, NUMBER_FORMAT, nullFormat, null );
      }

      case "FloodedArea": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.2", compNameStrings[1] ); //$NON-NLS-1$
        return new ComponentUiDecimalHandler( componentIndex, false, true, true, headerName, SWT.RIGHT, ColumnWidthInfo.PACK, -1, NUMBER_FORMAT, nullFormat, null );
      }

      case "AveragedDamage": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.3", compNameStrings[1] ); //$NON-NLS-1$
        return new ComponentUiDecimalHandler( componentIndex, false, true, true, headerName, SWT.RIGHT, ColumnWidthInfo.PACK, -1, NUMBER_FORMAT, nullFormat, null );
      }

      case "AnnualValue": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.4" ); //$NON-NLS-1$
        return new ComponentUiDoubleHandler( componentIndex, false, true, true, headerName, SWT.RIGHT, ColumnWidthInfo.PACK, -1, NUMBER_FORMAT, nullFormat, null );
      }

      case "TotalPotentialDamage": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.5" ); //$NON-NLS-1$
        return new ComponentUiDoubleHandler( componentIndex, false, true, true, headerName, SWT.RIGHT, ColumnWidthInfo.PACK, -1, NUMBER_FORMAT, nullFormat, null );
      }

      case "Landuse": //$NON-NLS-1$
      {
        final String headerName = Messages.getString( "org.kalypso.risk.widget.StatisticResultComposite.6" ); //$NON-NLS-1$
        return new ComponentUiStringHandler( componentIndex, false, true, true, headerName, SWT.NONE, ColumnWidthInfo.PACK, -1, "%s", nullFormat, null ); //$NON-NLS-1$
      }

      case "Group": //$NON-NLS-1$
      {
        final String headerName = Messages.getString("StatisticResultComponentProvider.0"); //$NON-NLS-1$
        return new ComponentUiStringHandler( componentIndex, false, true, true, headerName, SWT.NONE, ColumnWidthInfo.PACK, -1, "%s", nullFormat, null ); //$NON-NLS-1$
      }

      default:
        return null;
    }
  }
}