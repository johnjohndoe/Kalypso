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

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Feature control for editing the description of a simulation. The description of a simulation must be a filename
 * compatible string.
 * 
 * @author Holger Albert
 */
public class SimulationDescriptionFeatureControl extends AbstractFeatureControl
{
  public SimulationDescriptionFeatureControl( final IPropertyType ftp )
  {
    super( ftp );
  }

  public SimulationDescriptionFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
  }

  @Override
  protected Control createControl( final Composite parent, final int style )
  {
    final Action descriptionAction = new SimulationDescriptionAction( this );

    /* Create a button. */
    final Button descriptionButton = ActionButton.createButton( null, parent, descriptionAction );
    descriptionButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, false, false ) );

    return descriptionButton;
  }

  @Override
  public void updateControl( )
  {
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }

  void fireFeatureChanges( final ICommand changes )
  {
    if( changes != null )
      fireFeatureChange( changes );
  }
}