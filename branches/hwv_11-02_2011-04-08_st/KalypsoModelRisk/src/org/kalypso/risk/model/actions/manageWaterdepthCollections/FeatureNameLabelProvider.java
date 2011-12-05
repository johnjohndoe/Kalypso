package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import ogc31.www.opengis.net.gml.FileType;

import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public class FeatureNameLabelProvider extends GMLLabelProvider
{
  private static final String NAME_NOT_DEFINED = Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.FeatureNameLabelProvider.0" ); //$NON-NLS-1$

  @Override
  public String getText( final Object element )
  {
    if( element instanceof Feature )
    {
      final IAnnualCoverageCollection collection = (IAnnualCoverageCollection) ((Feature) element).getAdapter( IAnnualCoverageCollection.class );
      if( collection != null )
      {
        String name = collection.getName();
        if( name == null || name.length() == 0 )
          name = NAME_NOT_DEFINED;
        final Integer returnPeriod = collection.getReturnPeriod();
        if( returnPeriod != null && returnPeriod > 0 )
          name += " [" + returnPeriod.toString() + Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.FeatureNameLabelProvider.2" ); //$NON-NLS-1$ //$NON-NLS-2$
        else
          name += Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.FeatureNameLabelProvider.3" ); //$NON-NLS-1$
        return name;
      }
      final ICoverage coverage = (ICoverage) ((Feature) element).getAdapter( ICoverage.class );
      if( coverage != null && coverage instanceof RectifiedGridCoverage )
      {
        final StringBuffer label = new StringBuffer();
        final String name = coverage.getName();
        final Object rangeSet = ((RectifiedGridCoverage) coverage).getRangeSet();
        if( !(rangeSet instanceof FileType) )
          throw new UnsupportedOperationException( org.kalypso.risk.i18n.Messages.getString("org.kalypso.risk.model.actions.manageWaterdepthCollections.FeatureNameLabelProvider.1") ); //$NON-NLS-1$

        final String mimeType = ((FileType) rangeSet).getMimeType();
        if( name == null || name.length() == 0 )
          label.append( NAME_NOT_DEFINED );
        else
          label.append( name );
        final RectifiedGridDomain gridDomain = ((RectifiedGridCoverage) coverage).getGridDomain();
        label.append( " [" ); //$NON-NLS-1$
        if( mimeType != null && mimeType.length() > 0 )
          label.append( mimeType ).append( ", " ); //$NON-NLS-1$
        label.append( gridDomain.getNumColumns() ).append( "x" ).append( gridDomain.getNumRows() ); //$NON-NLS-1$
        label.append( ", " ); //$NON-NLS-1$
        label.append( Math.abs( gridDomain.getOffsetX().getGeoX() - gridDomain.getOffsetX().getGeoY() ) ).append( "x" ).append( Math.abs( gridDomain.getOffsetY().getGeoX() //$NON-NLS-1$
            - gridDomain.getOffsetY().getGeoY() ) );
        // try
        // {
        // // label.append( ", " ).append( gridDomain.getCoordinateSystem().getName() ); //$NON-NLS-1$
        // }
        // catch( RemoteException e )
        // {
        // }
        label.append( "]" ); //$NON-NLS-1$
        return label.toString();
      }
      return super.getText( element );
    }
    else
      return super.getText( element );
  }
}
