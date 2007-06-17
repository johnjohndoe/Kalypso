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
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dejan Antanaskovic
 */
public class TimestepFillerFeatureControl extends AbstractFeatureControl implements IFeatureControl
{
  private IFeatureControl m_featureControl;

  private final IFeatureSelectionListener m_fsl = new IFeatureSelectionListener()
  {
    public void selectionChanged( org.kalypso.ogc.gml.selection.IFeatureSelection selection )
    {
      System.out.println( "SELECTION CHANGEEEEEEEEEEEED!!!!!" );
    }
  };

  private final IFeatureChangeListener m_fcl = new IFeatureChangeListener()
  {
    public void featureChanged( final FeatureChange[] changes )
    {
      System.out.println( "featureChanged:" + changes.toString() );
      // final GMLWorkspace workspace = m_featureComposite.getFeature().getWorkspace();
      // final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, changes );
      //
      // m_target.setCommandManager( m_commandManager );
      // m_target.postCommand( command, null );
    }

    public void openFeatureRequested( final Feature feature, final IPropertyType ftp )
    {
      // just show this feature in the view, don't change the selection this doesn't work
      // don't change the command manager, changing the feature only work inside the same workspace
      // activateFeature( feature, false, null );
    }
  };

  public TimestepFillerFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
    // super.addChangeListener( m_fcl );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    System.out.println( "" );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Feature feature = getFeature();
    m_featureControl = new ButtonFeatureControl( feature, getFeatureTypeProperty() );
    m_featureControl.addChangeListener( m_fcl );
    final Button control = (Button) m_featureControl.createControl( parent, style );
    control.setText( "Zeitschritte definieren" );

    control.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Shell shell = display.getActiveShell();
        final TimeStepFillerWizard wizard = new TimeStepFillerWizard( getFeature() );
        final WizardDialog wizardDialog = new WizardDialog( shell, wizard );
        wizardDialog.open();
        final FeatureChange[] changes = wizard.getFeatureChange();
        fireFeatureChange( changes );
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
  public void removeModifyListener( final ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    final Feature feature = getFeature();
    // final IPropertyType featureTypeProperty = getFeatureTypeProperty();
    // final Feature f1 = ((XLinkedFeature_Impl) feature.getParent().getParent().getProperties()[0]).getFeature();

    // final Feature feature = getFeature().getParent().getParent();
    // final IPropertyType featureTypeProperty = getFeatureTypeProperty();
    // final Object property = feature.getProperty( getFeatureTypeProperty() );
    // // if( m_selector != null && property instanceof SplitSort )
    // {
    // final Feature f = ((XLinkedFeature_Impl) feature.getProperty( m_selector )).getFeature();
    // m_featureControl.setFeature( f );
    // }
    // // m_featureControl.setFeature( feature );
    // m_featureControl.updateControl();

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
