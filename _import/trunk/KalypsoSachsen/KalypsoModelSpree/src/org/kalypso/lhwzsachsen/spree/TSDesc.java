package org.kalypso.lhwzsachsen.spree;

public final class TSDesc
{
  public final String id;

  public final boolean output;

  public final String useMetadataFrom;

  public final boolean isRequired;

  public TSDesc( final String id )
  {
    this( id, false, null, false );
  }

  public TSDesc( final String id, final boolean output, final String useMetadataFrom, final boolean required )
  {
    this.id = id;
    this.output = output;
    this.useMetadataFrom = useMetadataFrom;
    this.isRequired = required;
  }
}