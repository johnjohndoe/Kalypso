
package org.kalypso.services.sensor.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.sensor.impl.jaxws.GetChildrenResponse;

@XmlRootElement(name = "getChildrenResponse", namespace = "http://impl.sensor.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getChildrenResponse", namespace = "http://impl.sensor.services.kalypso.org/")
public class GetChildrenResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.repository.service.ItemBean[] _return;

    /**
     * 
     * @return
     *     returns ItemBean[]
     */
    public org.kalypso.repository.service.ItemBean[] get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.repository.service.ItemBean[] _return) {
        this._return = _return;
    }

}
