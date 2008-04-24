package org.kalypso.ogc.gml.mapmodel.visitor;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;

public final class ThemeVisiblePredicate implements IKalypsoThemePredicate
{
  public boolean decide( final IKalypsoTheme theme )
  {
    return theme.isVisible();
  }
}
