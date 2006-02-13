
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.activation.DataHandler;
import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.GetSchemaResponse;

@XmlRootElement(name = "getSchemaResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getSchemaResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
public class GetSchemaResponse {

    @XmlElement(name = "return", namespace = "")
    private DataHandler _return;

    /**
     * 
     * @return
     *     returns DataHandler
     */
    public DataHandler get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(DataHandler _return) {
        this._return = _return;
    }

}
