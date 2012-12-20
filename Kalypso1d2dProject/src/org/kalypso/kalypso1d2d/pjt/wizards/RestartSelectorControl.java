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
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dejan Antanaskovic
 */
public class RestartSelectorControl extends AbstractFeatureControl
{
  public RestartSelectorControl( final Feature feature, final IPropertyType pt )
  {
    super( feature, pt );
  }

  @Override
  public Control createControl( final Composite parent, final int style )
  {
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectorControl.0" ) ); //$NON-NLS-1$

    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IControlModel1D2D controlModel = (IControlModel1D2D)getFeature().getAdapter( IControlModel1D2D.class );
        final RestartSelectWizard wizard = new RestartSelectWizard( controlModel );
        final WizardDialog wizardDialog = new WizardDialog( parent.getDisplay().getActiveShell(), wizard );
        wizardDialog.open();
      }
    } );

    return button;
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }

  @Override
  public void updateControl( )
  {
  }
}