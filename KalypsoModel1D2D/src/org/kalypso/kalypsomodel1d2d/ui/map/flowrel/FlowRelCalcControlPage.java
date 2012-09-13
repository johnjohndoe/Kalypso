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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.FeatureComposite;
import org.kalypso.ogc.gml.featureview.maker.CachedFeatureviewFactory;
import org.kalypso.ogc.gml.featureview.maker.FeatureviewHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class FlowRelCalcControlPage extends WizardPage
{
  private IFlowRelation1D[] m_flowRels;

  private TuhhCalculation m_template;

  private FeatureComposite m_featureComposite;

  private Group m_featureGroup;

  private ScrolledForm m_scrolledForm;

  public FlowRelCalcControlPage( final String pageName )
  {
    super( pageName );
  }

  public FlowRelCalcControlPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    m_scrolledForm = new ScrolledForm( parent );
    m_scrolledForm.setExpandHorizontal( true );
    setControl( m_scrolledForm );

    m_scrolledForm.getBody().setLayout( new GridLayout() );
    m_featureGroup = new Group( m_scrolledForm.getBody(), SWT.NONE );
    m_featureGroup.setLayout( new GridLayout() );
    final GridData groupData = new GridData( SWT.FILL, SWT.FILL, true, true );
    m_featureGroup.setLayoutData( groupData );
    m_featureGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcControlPage.0" ) ); //$NON-NLS-1$

    final CachedFeatureviewFactory featureviewFactory = new CachedFeatureviewFactory( new FeatureviewHelper() );
    featureviewFactory.addView( getClass().getResource( "resources/calcControlPage.gft" ) ); //$NON-NLS-1$
    featureviewFactory.addView( getClass().getResource( "resources/waterlevelParameter.gft" ) ); //$NON-NLS-1$
    m_featureComposite = new FeatureComposite( null, null, featureviewFactory );
    m_featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void featureChanged( final ICommand changeCommand )
      {
        try
        {
          // System.out.println( "Processing!!!" );
          changeCommand.process();

          m_featureComposite.updateControl();
        }
        catch( final Exception e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }

      @Override
      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );

  }

  public void setCalculation( final IFlowRelation1D[] flowRelation1Ds )
  {
    m_featureComposite.disposeControl();

    try
    {
      m_template = FlowRelCalcControlPage.createTemplateCalculation( flowRelation1Ds );
      m_flowRels = flowRelation1Ds;

      if( m_template == null )
      {
        // show error message
        setErrorMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcControlPage.3" ) ); //$NON-NLS-1$
      }
      else
      {
        // show template in feature control
        m_featureComposite.setFeature( m_template );
        m_featureComposite.createControl( m_featureGroup, SWT.NONE );
        m_featureGroup.layout();
        m_scrolledForm.reflow( true );
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      setErrorMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcControlPage.4" ) + e.toString() ); //$NON-NLS-1$
    }
  }

  public static TuhhCalculation createTemplateCalculation( final IFlowRelation1D[] flowRels ) throws Exception
  {
    if( flowRels.length == 0 )
      return null;

    // TODO: at the moment, all parameters are taken from the first flow relation; better: merge?
    final IFlowRelation1D flowRel = flowRels[0];
    final Feature flowRelFeature = flowRel;
    final GMLWorkspace flowRelWorkspace = flowRelFeature.getWorkspace();

    final TuhhCalculation calculation = flowRel.getCalculation();

    final TuhhWspmProject project = TuhhWspmProject.create( null, flowRelWorkspace.getFeatureProviderFactory() );
    if( calculation == null )
    {
      /* If no calculated existed before, use default values */
      return project.createReibConstCalculation();
    }

    /* If a calculation exists, copy its values */
    final IFeatureType featureType = project.getFeatureType();
    final IRelationType rt = (IRelationType) featureType.getProperty( TuhhWspmProject.QNAME_PROP_CALC_MEMBER );
    return (TuhhCalculation) FeatureHelper.cloneFeature( project, rt, calculation );
  }

  public TuhhCalculation getTemplate( )
  {
    return m_template;
  }

  public IFlowRelation1D[] getFlowRels( )
  {
    return m_flowRels;
  }

}
