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
package org.kalypso.ui.rrm.internal.simulations;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Holger Albert
 */
public class SimulationDescriptionAction extends Action
{
  private final SimulationDescriptionFeatureControl m_control;

  public SimulationDescriptionAction( final SimulationDescriptionFeatureControl control )
  {
    m_control = control;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Feature feature = m_control.getFeature();
    final String initialDescription = feature.getDescription();

    final Display display = event.widget.getDisplay();
    final Shell shell = display.getActiveShell();

    final InputDialog dialog = new InputDialog( shell, getText(), "Bitte geben Sie einen Namen ein", initialDescription, new SimulationDescriptionValidator( (NAControl) feature ) );
    final int open = dialog.open();
    if( open != Window.OK )
      return;

    final String newDescription = dialog.getValue();
    if( !ObjectUtils.equals( initialDescription, newDescription ) )
    {
      final IPropertyType pt = m_control.getFeatureTypeProperty();
      m_control.fireFeatureChanges( new ChangeFeatureCommand( feature, pt, newDescription ) );
    }
  }

  @Override
  public String getText( )
  {
    return "Rename";
  }
}