package org.kalypso.ogc.gml;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;

/**
 * @author vdoemming
 */
public interface IKalypsoFeatureTheme extends IKalypsoTheme
{
  public GMLWorkspace getWorkspace();
  
  public FeatureType getFeatureType();

  public void addStyle( final KalypsoUserStyle style );

  public void removeStyle( final KalypsoUserStyle style );

  public UserStyle[] getStyles();
  
  public FeatureList getFeatureList();
}
