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
package org.kalypso.model.wspm.tuhh.ui.panel.vegetation.pages;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.AbstractDatabinding;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.ui.pager.AbstractElementPage;
import org.kalypso.contribs.eclipse.ui.pager.IElementPage;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.Vegetations;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.util.vegetation.GuessVegetationClassesRunnable;
import org.kalypso.model.wspm.tuhh.ui.panel.roughness.utils.RoughnessesDataModel;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationPanelHelper;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.utils.VegetationsDataModel;
import org.kalypso.ui.editor.styleeditor.binding.SLDBinding;

/**
 * @author Dirk Kuch
 */
public class VegetationPropertiesPage extends AbstractElementPage implements IElementPage
{
  private static final String EMPTY_STRING = "<Variating>";

  protected final IProfil m_profile;

  private IDataBinding m_binding;

  public VegetationPropertiesPage( final IProfil profile )
  {
    super( VegetationPropertiesPage.class.getName() );
    m_profile = profile;
  }

  @Override
  public String getLabel( )
  {
    return "Vegetation Properties";
  }

  @Override
  public void render( final Composite body, final FormToolkit toolkit )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout( 4, false ) );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    group.setText( "Flow Zone Vegetations" );
    toolkit.adapt( group );

    m_binding = new AbstractDatabinding( toolkit )
    {
    };

    toolkit.createLabel( group, "" );
    toolkit.createLabel( group, "AX" ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) );
    toolkit.createLabel( group, "AY" ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) );
    toolkit.createLabel( group, "DP" ).setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) );

    // TODO validators
    build( group, toolkit, "Left Flood-Plain", RoughnessesDataModel.PROPERTY_LEFT_FLOODPLAIN, null );
    build( group, toolkit, "River Tube", RoughnessesDataModel.PROPERTY_RIVER_TUBE, null );
    build( group, toolkit, "Right Flood-Plain", RoughnessesDataModel.PROPERTY_RIGHT_FLOODPLAIN, null );

    final ImageHyperlink lnkRemove = toolkit.createImageHyperlink( group, SWT.NULL );
    lnkRemove.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false, 4, 0 ) );
    lnkRemove.setText( String.format( "Remove: %s", getLabel() ) );

    lnkRemove.addHyperlinkListener( new HyperlinkAdapter()
    {
      @Override
      public void linkActivated( final org.eclipse.ui.forms.events.HyperlinkEvent e )
      {
        VegetationPanelHelper.removeVegetationTypes( m_profile );
      }
    } );

    /** additional actions */
    if( hasActions() )
    {

      final Group grActions = new Group( body, SWT.NULL );
      grActions.setLayout( new GridLayout() );
      grActions.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
      grActions.setText( "Additional Actions" );

      final ImageHyperlink lnk = toolkit.createImageHyperlink( grActions, SWT.NULL );
      lnk.setText( "Guess vegetation classes from existing vegetation values" );
      lnk.addHyperlinkListener( new HyperlinkAdapter()
      {
        @Override
        public void linkActivated( final HyperlinkEvent e )
        {
          final boolean overwriteValues = MessageDialog.openQuestion( lnk.getShell(), "Overwrite", "Overwrite existing classes?" );

          final GuessVegetationClassesRunnable worker = new GuessVegetationClassesRunnable( m_profile, overwriteValues, Double.MAX_VALUE );
          ProgressUtilities.busyCursorWhile( worker );

        }
      } );

      toolkit.adapt( grActions );
      grActions.layout();
    }

    body.layout();

  }

  private boolean hasActions( )
  {
    return Vegetations.hasVegetationClass( m_profile );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.pager.IElementPage#dispose()
   */
  @Override
  public void dispose( )
  {
  }

  protected void build( final Composite body, final FormToolkit toolkit, final String label, final String property, final IValidator validator )
  {
    toolkit.createLabel( body, label );

    final VegetationsDataModel modelAx = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) );
    final VegetationsDataModel modelAy = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ) );
    final VegetationsDataModel modelDp = new VegetationsDataModel( m_profile, m_profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ) );

    final Text ax = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    ax.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    ax.setMessage( EMPTY_STRING );

    bind( ax, modelAx, property, validator );

    final Text ay = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    ay.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    ay.setMessage( EMPTY_STRING );

    bind( ay, modelAy, property, validator );

    final Text dp = toolkit.createText( body, StringUtils.EMPTY, SWT.BORDER | SWT.RIGHT );
    dp.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    dp.setMessage( EMPTY_STRING );

    bind( dp, modelDp, property, validator );
  }

  protected void bind( final Text text, final VegetationsDataModel model, final String property, final IValidator validator )
  {
    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SLDBinding.TEXT_DEFAULT_EVENTS );
    final IObservableValue modelValue = model.getObservableValue( property );

    final DataBinder binder = new DataBinder( targetValue, modelValue );

    m_binding.bindValue( binder );
  }

}
