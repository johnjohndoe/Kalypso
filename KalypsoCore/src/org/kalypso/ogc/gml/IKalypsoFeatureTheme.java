package org.kalypso.ogc.gml;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author vdoemming
 */
public interface IKalypsoFeatureTheme extends IKalypsoTheme, ICommandTarget
{
  public CommandableWorkspace getWorkspace();
  
  public ISchedulingRule getSchedulingRule();
  
  public FeatureType getFeatureType();

  public void addStyle( final KalypsoUserStyle style );

  public void removeStyle( final KalypsoUserStyle style );

  public UserStyle[] getStyles();
  
  public FeatureList getFeatureList();
}
