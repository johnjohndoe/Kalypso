package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.swt.widgets.Shell;

/**
 * @author belger
 */
public interface IFeatureDialog
{
  public int open( final Shell shell );

  public void collectChanges( final Collection c );

  public String getLabel();
}
