package com.bce.eind.core.profil.validator;

import org.eclipse.core.runtime.CoreException;

public interface IValidatorMarkerCollector
{
  public static final String MARKER_ATTRIBUTE_POINTPOS = "profile.marker.attribute.pointpos";
  public static final String MARKER_ATTRIBUTE_POINTPROPERTY = "profile.marker.attribute.pointpos";

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method,
   * so changes in the implementation (e.g. the type of the marker) are reflekted on all rules.
   * 
   * @throws CoreException
   */
  public void createProfilMarker( final boolean isSevere, final String message,
      final String description, final int pointPos, final String pointProperty ) throws CoreException;

  /**
   * Clear all markers which may apply to this collector
   * 
   * @throws CoreException
   */
  public void reset( ) throws CoreException;
}
