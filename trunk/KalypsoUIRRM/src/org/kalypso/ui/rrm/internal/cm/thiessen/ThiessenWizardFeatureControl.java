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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.rcm.binding.IThiessenStation;
import org.kalypso.model.rcm.util.IBoundaryCalculator;
import org.kalypso.model.rcm.util.ThiessenAreaJob;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.command.FeatureChangeModellEvent;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class ThiessenWizardFeatureControl extends AbstractFeatureControl
{
  private final ModellEventListener m_modelListener = new ModellEventListener()
  {
    @Override
    public void onModellChange( final ModellEvent modellEvent )
    {
      handleModellChange( modellEvent );
    }
  };

  private final IJobChangeListener m_jobListener = new JobChangeAdapter()
  {
    @Override
    public void done( final IJobChangeEvent event )
    {
      handleThiessenDone( (ThiessenAreaJob) event.getJob() );
    }
  };

  private ThiessenAreaJob m_thiessenJob;

  private final Geometry m_catchmentsExtent;

  public ThiessenWizardFeatureControl( final Feature feature, final IPropertyType pt )
  {
    super( feature, pt );

    if( feature != null )
      feature.getWorkspace().addModellListener( m_modelListener );

      m_catchmentsExtent = calculateCatchmentsExtent();

    restartThiessenJob();
  }

  private Geometry calculateCatchmentsExtent( )
  {
    try
    {
      final IScenarioDataProvider scenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

      final GeometryFactory gf = new GeometryFactory();

      final Collection<Geometry> patches = new ArrayList<>();

      final NaModell model = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_MODEL );
      final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();
      for( final Catchment catchment : catchments )
      {
        try
        {
          final GM_Polygon geometry = catchment.getGeometry();
          patches.add( JTSAdapter.export( geometry ) );
        }
        catch( final GM_Exception e )
        {
          e.printStackTrace();
        }
      }

      final GeometryCollection allPatches = gf.createGeometryCollection( patches.toArray( new Geometry[patches.size()] ) );
      return allPatches.convexHull();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public void setFeature( final Feature feature )
  {
    final Feature oldFeature = getFeature();
    if( oldFeature != null )
      oldFeature.getWorkspace().removeModellListener( m_modelListener );

    super.setFeature( feature );

    if( feature != null )
      feature.getWorkspace().addModellListener( m_modelListener );

    restartThiessenJob();
  }

  @Override
  public void dispose( )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.getWorkspace().removeModellListener( m_modelListener );

    super.dispose();
  }

  @Override
  public Control createControl( final FormToolkit toolkit, final Composite parent, final int style )
  {
    final Composite composite = toolkit.createComposite( parent );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( composite );

    final IAction selectAllAction = new SelectAllAction( this, Boolean.TRUE );
    final Button selectAllButton = ActionButton.createButton( toolkit, composite, selectAllAction );
    setButtonData( selectAllButton );

    final IAction deselectAllAction = new SelectAllAction( this, Boolean.FALSE );
    final Button deselectAllButton = ActionButton.createButton( toolkit, composite, deselectAllAction );
    setButtonData( deselectAllButton );

    return composite;
  }

  private void setButtonData( final Button button )
  {
    final GC gc = new GC( button );
    gc.setFont( JFaceResources.getDialogFont() );
    final FontMetrics fontMetrics = gc.getFontMetrics();
    gc.dispose();

    final GridData data = new GridData( SWT.FILL, SWT.CENTER, true, false );
    final int widthHint = Dialog.convertHorizontalDLUsToPixels( fontMetrics, 30 );
    final Point minSize = button.computeSize( SWT.DEFAULT, SWT.DEFAULT, true );
    data.widthHint = Math.max( widthHint, minSize.x );
    button.setLayoutData( data );
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

  protected void handleModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureChangeModellEvent )
    {
      final FeatureChangeModellEvent fcme = (FeatureChangeModellEvent) modellEvent;
      final FeatureChange[] changes = fcme.getChanges();
      final boolean shouldUpdate = FeatureUtils.checkChange( getFeature(), changes, IThiessenStation.PROPERTY_ACTIVE );
      if( !shouldUpdate )
        return;

      restartThiessenJob();
    }
  }

  private void restartThiessenJob( )
  {
    // reschedule last thiessen job
    if( m_thiessenJob != null )
      m_thiessenJob.cancel();

    final Feature feature = getFeature();
    if( feature == null )
      return;

    final GMLWorkspace workspace = feature.getWorkspace();
    final IPropertyType featureTypeProperty = getFeatureTypeProperty();
    final List< ? > ombroFeatures = (List< ? >) feature.getProperty( featureTypeProperty );

    final IBoundaryCalculator bufferCalculator = new UnionBoundaryCalculator( m_catchmentsExtent, 0.05 );

    // TODO: rather an operation inside the wizard context?
    m_thiessenJob = new ThiessenAreaJob( true, bufferCalculator, workspace, ombroFeatures, IThiessenStation.PROPERTY_LOCATION, IThiessenStation.PROPERTY_THIESSEN_AREA, IThiessenStation.PROPERTY_ACTIVE );
    m_thiessenJob.setUser( true );
    m_thiessenJob.addJobChangeListener( m_jobListener );

    m_thiessenJob.schedule( 250 );
  }

  void changeIsUsed( final Boolean active )
  {
    final List<FeatureChange> changes = new ArrayList<>();

    final List< ? > stations = (List< ? >) getFeature().getProperty( getFeatureTypeProperty() );
    for( final Object object : stations )
    {
      final FeatureChange change = new FeatureChange( (Feature) object, IThiessenStation.PROPERTY_ACTIVE, active );
      changes.add( change );
    }

    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( getFeature().getWorkspace(), changes.toArray( new FeatureChange[changes.size()] ) );
    fireFeatureChange( command );
  }

  protected void handleThiessenDone( final ThiessenAreaJob job )
  {
    final ICommand change = job.getChange();
    if( change != null )
      fireFeatureChange( change );
  }

}