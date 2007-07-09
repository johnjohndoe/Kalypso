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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.io.File;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.ButtonFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RestartSelectorControl extends AbstractFeatureControl implements IFeatureControl
{
  private IFeatureControl m_featureControl;

  private final IFeatureChangeListener m_fcl = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange[] changes )
    {
    }

    public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
    {
    }
  };

  public RestartSelectorControl( Feature feature, IPropertyType pt )
  {
    super( feature, pt );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( ModifyListener l )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( Composite parent, int style )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Feature feature = getFeature();
    m_featureControl = new ButtonFeatureControl( feature, getFeatureTypeProperty() );
// m_featureControl = new ButtonFeatureControl( feature, feature.getFeatureType().getProperty( new QName("result") ) );
    m_featureControl.addChangeListener( m_fcl );
    final Button control = (Button) m_featureControl.createControl( parent, style );
    control.setText( "Select restart file(s)" );

    control.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Shell shell = display.getActiveShell();
        final RestartSelectWizard wizard = new RestartSelectWizard( getFeature() );
        final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
        wizardDialog.open();
        final File[] changes = wizard.getSelectedFiles();
      }
    } );
    return control;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#setFeature(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public void setFeature( final Feature feature )
  {
    // TODO Auto-generated method stub
    super.setFeature( feature );
  }
}
