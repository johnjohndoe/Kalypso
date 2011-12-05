package org.kalypso.model.flood.util;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * Label provider is to presetn runoff events before they get processed.
 * 
 * @author Thomas Jung
 * @author Gernot Belger
 */
public final class RunoffEventForProcessingLabelProvider extends LabelProvider
{
  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    final IRunoffEvent event = (IRunoffEvent) element;

    final ICoverageCollection resultCoverages = event.getResultCoverages();
    if( resultCoverages != null && resultCoverages.getCoverages().size() > 0 )
      return Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.18", event.getName() ); //$NON-NLS-1$

    return getEventLabel( event );
  }

  public static String getEventLabel( final IRunoffEvent event )
  {
    final String name = event.getName();
    if( StringUtils.isBlank( name ) )
      Messages.getString( "org.kalypso.model.flood.util.FloodModelHelper.24", event.getGmlID() ); //$NON-NLS-1$

    return name;
  }
}