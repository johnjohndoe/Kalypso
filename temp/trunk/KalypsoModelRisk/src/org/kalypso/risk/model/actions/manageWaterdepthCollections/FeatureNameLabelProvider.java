package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.rmi.RemoteException;

import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

public class FeatureNameLabelProvider extends GMLLabelProvider
{
  private static final String NAME_NOT_DEFINED = "<name not defined>";

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
          name += " [" + returnPeriod.toString() + " year flood]";
        else
          name += " [return period not specified]";
        return name;
      }
      final ICoverage coverage = (ICoverage) ((Feature) element).getAdapter( ICoverage.class );
      if( coverage != null && coverage instanceof RectifiedGridCoverage )
      {
        final StringBuffer label = new StringBuffer();
        final String name = coverage.getName();
        final String mimeType = ((RectifiedGridCoverage) coverage).getRangeSet().getFile().getMimeType();
        if( name == null || name.length() == 0 )
          label.append( NAME_NOT_DEFINED );
        else
          label.append( name );
        final RectifiedGridDomain gridDomain = ((RectifiedGridCoverage) coverage).getGridDomain();
        label.append( " [" );
        if( mimeType != null && mimeType.length() > 0 )
          label.append( mimeType ).append( ", " );
        label.append( gridDomain.getNumColumns() ).append( "x" ).append( gridDomain.getNumRows() );
        label.append( ", " );
        label.append( Math.abs( gridDomain.getOffsetX().getGeoX() - gridDomain.getOffsetX().getGeoY() ) ).append( "x" ).append( Math.abs( gridDomain.getOffsetY().getGeoX()
            - gridDomain.getOffsetY().getGeoY() ) );
        try
        {
          label.append( ", " ).append( gridDomain.getCoordinateSystem().getName() );
        }
        catch( RemoteException e )
        {
        }
        label.append( "]" );
        return label.toString();
      }
      return super.getText( element );
    }
    else
      return super.getText( element );
  }
}
