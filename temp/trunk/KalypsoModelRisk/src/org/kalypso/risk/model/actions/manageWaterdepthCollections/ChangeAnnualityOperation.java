package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class ChangeAnnualityOperation implements ICoreRunnableWithProgress
{
  private final IRasterDataModel m_model;

  private final AbstractCascadingLayerTheme m_wspThemes;

  private final SzenarioDataProvider m_provider;

  private final int m_newReturnPeriod;

  private final IAnnualCoverageCollection m_coverageCollection;

  public ChangeAnnualityOperation( final Object selection, final int returnPeriod, final IRasterDataModel model, final AbstractCascadingLayerTheme wspThemes, final SzenarioDataProvider provider )
  {
    m_coverageCollection = (IAnnualCoverageCollection) ((Feature) selection).getAdapter( IAnnualCoverageCollection.class );
    m_newReturnPeriod = returnPeriod;
    m_model = model;
    m_wspThemes = wspThemes;
    m_provider = provider;
  }

  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      monitor.beginTask( Messages.getString("ChangeAnnualityOperation.0"), 7 ); //$NON-NLS-1$
      final int oldReturnPeriod = m_coverageCollection.getReturnPeriod();
      m_coverageCollection.setReturnPeriod( m_newReturnPeriod );
      m_coverageCollection.setName( "HQ " + m_newReturnPeriod ); //$NON-NLS-1$
      updateTheme( m_wspThemes, oldReturnPeriod );

      final GMLWorkspace workspace = m_model.getFeature().getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_coverageCollection.getFeature().getParent(), new Feature[] { m_coverageCollection.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      m_provider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      m_provider.saveModel( IRasterDataModel.class, new SubProgressMonitor( monitor, 1 ) );
      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  protected void updateTheme( final AbstractCascadingLayerTheme parentKalypsoTheme, final int oldReturnPeriod ) throws Exception
  {
    final IKalypsoTheme[] themes = parentKalypsoTheme.getAllThemes();
    boolean themeFound = false;
    for( int i = 0; i < themes.length; i++ )
      if( themes[i].getName().equals( "HQ " + oldReturnPeriod ) ) //$NON-NLS-1$
      {
        themes[i].setName( "HQ " + m_newReturnPeriod ); //$NON-NLS-1$
        themeFound=true;
        break;
      }
    // if theme had no annuality before (z.B. imported from flood modeler), create new layer
    if(!themeFound){
      final StyledLayerType layer = new StyledLayerType();
      layer.setName( "HQ "+ m_newReturnPeriod); //$NON-NLS-1$
      layer.setFeaturePath( "#fid#" + m_coverageCollection.getGmlID() + "/coverageMember" ); //$NON-NLS-1$ //$NON-NLS-2$
      layer.setLinktype( "gml" ); //$NON-NLS-1$
      layer.setType( "simple" ); //$NON-NLS-1$
      layer.setVisible( true );
      layer.setActuate( "onRequest" ); //$NON-NLS-1$
      layer.setHref( "../models/RasterDataModel.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
      layer.setVisible( true );
      final Property layerPropertyDeletable = new Property();
      layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
      layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$
      final Property layerPropertyThemeInfoId = new Property();
      layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
      layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Wassertiefe %.2f m" ); //$NON-NLS-1$
      final List<Property> layerPropertyList = layer.getProperty();
      layerPropertyList.add( layerPropertyDeletable );
      layerPropertyList.add( layerPropertyThemeInfoId );
      final List<Style> styleList = layer.getStyle();
      final Style style = new Style();
      style.setLinktype( "sld" ); //$NON-NLS-1$
      style.setStyle( "Kalypso style" ); //$NON-NLS-1$
      style.setActuate( "onRequest" ); //$NON-NLS-1$
      style.setHref( "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
      style.setType( "simple" ); //$NON-NLS-1$
      styleList.add( style );

      parentKalypsoTheme.addLayer( layer );
    }
  }
}
