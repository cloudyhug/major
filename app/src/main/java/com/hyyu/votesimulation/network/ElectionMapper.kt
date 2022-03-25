package com.hyyu.votesimulation.network

import com.hyyu.votesimulation.model.Blog
import com.hyyu.votesimulation.model.Election
import com.hyyu.votesimulation.network.response.BlogObjectResponse
import com.hyyu.votesimulation.network.response.ElectionInfoObjectResponse
import com.hyyu.votesimulation.util.mapper.EntityMapper
import javax.inject.Inject

class ElectionMapper
@Inject
constructor() : EntityMapper<ElectionInfoObjectResponse, Election> {
    override fun mapFromEntity(entity: ElectionInfoObjectResponse): Election {
        return Election(
            id = entity.id,
            name = entity.name,
            isRunning = entity.isRunning
        )
    }

    override fun mapToEntity(domainModel: Election): ElectionInfoObjectResponse {
        return ElectionInfoObjectResponse(
            id = domainModel.id,
            name = domainModel.name,
            isRunning = domainModel.isRunning
        )
    }

    fun mapFromEntityList(entities: List<ElectionInfoObjectResponse>): List<Election> {
        return entities.map { mapFromEntity(it) }
    }

}